from pathlib import Path
from threading import Lock
import webbrowser

import uvicorn
from fastapi import APIRouter, FastAPI, HTTPException, Response
from fastapi.staticfiles import StaticFiles
from sqlalchemy import select
from sqlalchemy.ext.asyncio import create_async_engine
from sqlmodel import SQLModel
from sqlmodel.ext.asyncio.session import AsyncSession

from utils import rewrite_links
import utils_emacs

mutex = Lock()

app = FastAPI(
    openapi_url="/orc/openapi.json", docs_url="/orc/docs", redoc_url="/orc/redoc"
)


@app.get("/select")
def select_node():
    node = utils_emacs.select_node()
    id = node["id"]
    title = node["title"]

    redirect_url = f"/node/{id}"

    # we could return a redirect response:
    # return Response(status_code=303, headers={"Location": redirect_url})

    # but more useful to show link that user can click on, or drag and drop
    # but they can also reload the page to select another node
    html = f"""
<a href="{redirect_url}" target="_blank">{title}</a>
<br>
Ctrl-R to select another node
"""
    return Response(content=html, media_type="text/html")


CSS = """
html {
    font-family: system-ui, sans-serif;
    text-rendering: optimizeLegibility;
}

blockquote {
  margin-inline-start: 2rem;
  margin-inline-end: 0;
  margin-block: 2rem;
  padding: 0.4rem 0.8rem;
  border-inline-start: 0.35rem solid var(--accent);
  color: var(--text-light);
  font-style: italic;
}
"""


@app.get("/node.css")
async def get_node_css():
    return Response(content=CSS, media_type="text/css")


@app.get("/node/{or_node_id}")
def get_or_node_details(or_node_id: str):
    det = utils_emacs.get_or_node_details(or_node_id)

    # - add title, because obsidian shows that above the little block as well
    # - full file:/// links in this webview don't do anything if you click on them, but e.g. /bleh/
    #   will go to localhost:8000/bleh/ (ctrl-click to open with system)
    #   - make special endpoint that will cause our backend to open the file with system mime handler
    html = f"""
<html lang="en"><head><title>{det["title"]}</title><link rel="stylesheet" href="/node.css" /><body>
[<a href="/os-open/?filename={det["file"]}">{Path(det["file"]).name}</a>]
{det["html"]}
</body></html>
"""
    return Response(content=html, media_type="text/html")


@app.get("/os-open/")
def open(filename: str):
    webbrowser.open(filename)
    return "Requested system to open file with associated app."


# / on Linux, c:/ or the drive from where you're running this script
# I want orc-files to be able to accept full path names
root = Path.cwd().anchor
# /orc-files/ is mounted all by itself
app.mount("/orc-files", StaticFiles(directory=root), name="orc-files")

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000)
