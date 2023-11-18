from pathlib import Path
from urllib.parse import urlparse, urlunparse

from bs4 import BeautifulSoup


def _rewrite_link(link: str, dir: Path) -> str:
    if link is None:
        return None

    # parse URL into <scheme>://<netloc>/<path>;<params>?<query>#<fragment>
    parsed_url = urlparse(link)
    if parsed_url.netloc == "" and parsed_url.scheme in ["", "file"]:
        # with no netloc, path should be a file the backend can resolve locally
        # dir_path contains the file with the links, and links can be relative to that
        link_path = dir / Path(parsed_url.path)
        # rewrite link to /orc-files/ which will be mapped to /api/orc-files/
        new_url = urlunparse(
            (
                "",
                "",
                f"/orc-files/{link_path}",
                "",
                "",
                "",
            )
        )
        return new_url

    else:
        return None


def rewrite_links(html: str, dir: Path):
    """
    Rewrite links in HTML to point to local files in the orc-files directory.

    Parameters
    ----------
    dir : Path
        Path to the directory containing the org / HTML file.
    """
    soup = BeautifulSoup(html, "html.parser")

    num_rewrote = 0
    for elem, attr in [("a", "href"), ("img", "src")]:
        for link in soup.find_all(elem):
            attr_val = link.get(attr)

            if new_url := _rewrite_link(attr_val, dir):
                # set the href attribute of the anchor tag to the new URL
                link[attr] = new_url
                num_rewrote += 1

    return str(soup) if num_rewrote > 0 else html
