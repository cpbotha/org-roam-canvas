from datetime import datetime
from typing import List, Optional

from sqlalchemy import select, Column, DateTime
from sqlmodel import Relationship, create_engine, Field, Session, SQLModel


#--------------------------------------------------------------
# canvas

# we need four models so that schema is correctly interpreted between database,
# POST and GET requests. Each of these has different optional / required fields.
# See https://sqlmodel.tiangolo.com/tutorial/fastapi/multiple-models/
class CanvasBase(SQLModel):
    name: str = Field(unique=True)

# table=True is passed to the parent __init__subclass__()
# https://docs.python.org/3/reference/datamodel.html#object.__init_subclass__
class Canvas(CanvasBase, table=True):
    id: Optional[int] = Field(default=None, primary_key=True)
    created_at: Optional[datetime] = Field(default=None, sa_column=Column(DateTime, default=datetime.utcnow))
    nodes: List["Node"] = Relationship(back_populates="canvas")
    edges: List["Edge"] = Relationship(back_populates="canvas")

class CanvasCreate(CanvasBase):
    pass

class CanvasRead(CanvasBase):
    id: int

class CanvasUpdate(SQLModel):
    name: Optional[str] = None

#--------------------------------------------------------------
# nodes

class NodeBase(SQLModel):
    title: str
    # link takes preference
    link: Optional[str]
    # this can be markdown
    contents: Optional[str]
    x: int
    y: int
    width: int
    height: int
    colour: Optional[str]
    canvas_id: int = Field(default=None, foreign_key="canvas.id")

class Node(NodeBase, table=True):
    id: Optional[int] = Field(default=None, primary_key=True)
    created_at: Optional[datetime] = Field(default=None, sa_column=Column(DateTime, default=datetime.utcnow))
    canvas: Canvas = Relationship(back_populates="nodes")

class NodeRead(NodeBase):
    id: int

class NodeCreate(NodeBase):
    pass

class NodeUpdate(SQLModel):
    title: Optional[str] = None
    link: Optional[str] = None
    contents: Optional[str] = None
    x: Optional[int] = None
    y: Optional[int] = None
    width: Optional[int] = None
    height: Optional[int] = None
    colour: Optional[str] = None



# --------------------------------------------------------------
# edges

class EdgeBase(SQLModel):
    canvas_id: int = Field(default=None, foreign_key="canvas.id")
    node_from_id: int = Field(default=None, foreign_key="node.id")
    node_from_anchor: int
    node_to_id: int = Field(default=None, foreign_key="node.id")
    node_to_anchor: int


class Edge(EdgeBase, table=True):
    id: Optional[int] = Field(default=None, primary_key=True)
    created_at: Optional[datetime] = Field(default=None, sa_column=Column(DateTime, default=datetime.utcnow))

    canvas: Canvas = Relationship(back_populates="edges")
    # see https://github.com/tiangolo/sqlmodel/issues/10
    # we have to do extra work due to multiple relations between the same objects
    # if we want nodes to have incoming / outgoing edges, we have to set them up in both directions
    # see https://github.com/tiangolo/sqlmodel/issues/10#issuecomment-1537445078
    node_from: Node = Relationship(sa_relationship_kwargs={"primaryjoin": "Edge.node_from_id==Node.id"})
    node_to: Node = Relationship(sa_relationship_kwargs={"primaryjoin": "Edge.node_to_id==Node.id"})

class EdgeRead(EdgeBase):
    id: int

class EdgeCreate(EdgeBase):
    pass

class EdgeUpdate(SQLModel):
    node_from_id: Optional[int] = None
    node_from_anchor: Optional[int] = None
    node_to_id: Optional[int] = None
    node_to_anchor: Optional[int] = None
