import logging
import re
import subprocess
import tempfile
from pathlib import Path
from threading import Lock

from fastapi import HTTPException
from .utils import rewrite_links

mutex = Lock()

# had to add the 4MB END to limit the size of stdin we're reading in
# surprising, because it was not always necessary
# see my comment: https://www.reddit.com/r/emacs/comments/asil1y/comment/jm3r2a6/
ELISP_UNQUOTE = '(progn (insert-file-contents "/dev/stdin" nil nil 4000000) (princ (read (buffer-string))))'


def unquote_emacsclient_eval_output(output: str) -> str:
    """Clean up the stdout output from emacsclient.

    Notes
    -----
    - This solution:
      https://www.reddit.com/r/emacs/comments/asil1y/comment/eguo08l/
      https://emacs.stackexchange.com/a/28668/8743
    - More context on why emacsclient --eval is quoting its output:
      https://github.com/grettke/ebse
    """

    ret = subprocess.run(
        ["emacs", "-Q", "--batch", "--eval", ELISP_UNQUOTE],
        capture_output=True,
        text=True,
        input=output,
    )
    if ret.stderr:
        raise HTTPException(
            status_code=404,
            detail=f"Unable to unquote emacsclient output using emacs --batch: {ret.stderr}",
        )
    else:
        return ret.stdout


def ask_emacs(elisp: str, create_frame=False) -> str:
    """Ask emacsclient to evaluate elisp and return the result as unquoted string."""

    # no-wait means we get the value as soon as available, frame will stick around
    # without no-wait, user has to close emacs before we get value
    cmd = ["emacsclient"]
    if create_frame:
        cmd.append("-c")
        cmd.append("--no-wait")

    # we have to do this nasty non-context-manager thing because Windows will otherwise
    # not allow the subprocess (emacs) to write to the file
    tmp = tempfile.NamedTemporaryFile(delete=False)
    tmp.close()

    # on windows, replace backslashes with forward slashes
    fn = Path(tmp.name).as_posix()
    print(f"{tmp.name} --> {fn}")

    cmd.extend(["--eval", f'(write-region {elisp} nil "{fn}")'])

    # serialize access to emacs, else we would often get empty output when searching for a node id
    with mutex:
        ret = subprocess.run(cmd, capture_output=True, text=True)

    if ret.stderr:
        raise HTTPException(status_code=404, detail=ret.stderr)

    # when going via stdout, we would have to do this special emacs unquote
    # but now we're writing emacs output to tempfile, so unquoting not required
    # unquoted = unquote_emacsclient_eval_output(ret.stdout)
    # if unquoted == "nil":
    #     logging.error(f"ask_emacs error: elisp -> {ret.stdout} -> {unquoted}")
    #     raise HTTPException(status_code=404, detail="emacsclient returned nil")

    p = Path(tmp.name)
    output = p.open(mode="rb").read().decode("utf-8")
    p.unlink()
    return output


# we get literal "s back at start and finish of return
# also, what happens with \n is hard to predict
# so here we choose for |---| as separator
ELISP_SN = """
(progn
  (org-roam-node-find)
  (let ((node (org-roam-node-at-point)))
    (format "id:%s
title:%s
file:%s" (org-roam-node-id node) (org-roam-node-title node) (org-roam-node-file node)))
)"""


def select_node():
    """Find an org-roam node interactively."""
    # execute emacsclient to ask it for details about the org-roam node with or_node_id
    output = ask_emacs(ELISP_SN, create_frame=True)

    output_dict = None
    lines = output.split("\n")
    if mo_i := re.match("id:(.+)", lines[0]):
        if mo_t := re.match("title:(.+)", lines[1]):
            if mo_f := re.match("file:(.+)", lines[2]):
                output_dict = {
                    "id": mo_i.group(1),
                    "title": mo_t.group(1),
                    "file": mo_f.group(1),
                }

    if output_dict is None:
        raise HTTPException(status_code=404, detail="No node selected")
    else:
        return output_dict


# old style: full document, search and return <body> only without <body> tags
# this includes the title in a <h1>...</h1> block
# but it also includes the footer
ELISP_GND_EXTRACT_BODY = """(let ((fnpos (org-roam-id-find "{node_id}")))
  (when fnpos
    (with-temp-buffer
      (insert-file-contents (car fnpos))
      (goto-char (cdr fnpos))
      (let* ((node (org-roam-node-at-point))
             (html (org-export-as 'html (org-at-heading-p) nil nil))
             (start (cl-search "<body>" html))
             (end (cl-search "</body>" html))
             (body (substring html (+ start 6) end)))
        (format "title:%s
file:%s
%s" (org-roam-node-title node) (org-roam-node-file node) body)
        ))))
"""

# new style: tell emacs to do a body-only export, excluding <body> tags
# this does NOT include the title in a <h1>
# if you see empty topic links, try org-roam-update-org-id-locations
ELISP_GND_TEMP_BUFFER = """(let ((fnpos (org-roam-id-find "{node_id}")))
  (when fnpos
    (with-temp-buffer
      (insert-file-contents (car fnpos))
      (goto-char (cdr fnpos))
      (let* ((node (org-roam-node-at-point))
             (html (org-export-as 'html (org-at-heading-p) nil t)))
        (format "title:%s
file:%s
%s" (org-roam-node-title node) (org-roam-node-file node) html)
        ))))
"""

# newer style re-use existing buffer
# this applies emacs loading logic and hooks so that even mdroam files can be handled
# it re-uses the buffer if it's already open, so it can be faster
# TODO: in case of markdown, use markdown-live-preview-export, then get that
#       buffer's contents. at this moment, we're just getting the raw markdown.
#       Not many people use mdroam, so mostly only my problem
ELISP_GND = """(let ((fnpos (org-roam-id-find "{node_id}")))
  (when fnpos
    (let ((buffer (find-file-noselect (car fnpos))))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (cdr fnpos))
          (let* ((node (org-roam-node-at-point))
                 (html (org-export-as 'html (org-at-heading-p) nil t)))
            (format "title:%s
file:%s
%s" (org-roam-node-title node) (org-roam-node-file node) html)))))))
"""


def get_or_node_details(or_node_id: str):
    """Given an org-roam node ID, find its title and contents."""
    # execute emacsclient to ask it for details about the org-roam node with or_node_id
    output = ask_emacs(ELISP_GND.format(node_id=or_node_id))

    lines = output.split("\n")
    output_dict = {}
    if mo_t := re.match("title:(.+)", lines[0]):
        output_dict["title"] = mo_t.group(1)

        if mo_f := re.match("file:(.+)", lines[1]):
            output_dict["file"] = fn = mo_f.group(1)

            html = "\n".join(lines[2:])

            # rewrite local links in the html
            # print(html)
            output_dict["html"] = rewrite_links(html, Path(fn).parent)

            return output_dict

    else:
        logging.error(f"Unable to parse output from emacsclient: {output}")
        raise HTTPException(status_code=404, detail="No node found")
