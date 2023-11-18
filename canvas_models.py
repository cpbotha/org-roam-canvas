# I made these pydantic models for the Obsidian .canvas format, because for a
# short while I thought I wanted have the org-roam-canvas BE support .canvas
# files instead of the current prototype sqlite database

# should correspond with https://github.com/obsidianmd/obsidian-api/blob/master/canvas.d.ts
# got started with ChatGPT 4 on 2023-11-18, then had to complete and fix with copilot assistance

from enum import Enum
from typing import List, Literal, Optional, Union

from pydantic import BaseModel


class Side(str, Enum):
    TOP = "top"
    RIGHT = "right"
    BOTTOM = "bottom"
    LEFT = "left"


class EdgeEnd(str, Enum):
    NONE = "none"
    ARROW = "arrow"


class CanvasNodeData(BaseModel):
    id: str
    x: float
    y: float
    width: float
    height: float
    color: str


class BackgroundStyle(str, Enum):
    COVER = "cover"
    RATIO = "ratio"
    REPEAT = "repeat"


class CanvasGroupData(CanvasNodeData):
    type: Literal["group"]
    label: Optional[str] = None
    background: Optional[str] = None
    backgroundStyle: Optional[BackgroundStyle] = BackgroundStyle.COVER


class CanvasEdgeData(BaseModel):
    id: str
    fromNode: str
    fromSide: Side
    fromEnd: EdgeEnd = None
    toNode: str
    toSide: Side
    toEnd: EdgeEnd = None
    color: str
    label: str


class CanvasFileData(CanvasNodeData):
    type: Literal["file"]
    file: str


class CanvasTextData(CanvasNodeData):
    type: Literal["text"]
    text: str


class CanvasLinkData(CanvasNodeData):
    type: Literal["link"]
    url: str


# we don't support CanvasGroupData yet
AllCanvasNodeData = Union[
    CanvasFileData, CanvasTextData, CanvasLinkData, CanvasGroupData
]


class CanvasData(BaseModel):
    nodes: List[AllCanvasNodeData]
    edges: List[CanvasEdgeData]


def read_canvas_file(filename: str) -> CanvasData:
    """Parse a canvas file into a CanvasData object"""
    return CanvasData.parse_file(filename)


def write_canvas_file(canvas: CanvasData, filename: str):
    """Write CanvasData to json file filename"""
    canvas.json(indent=2, path=filename)
