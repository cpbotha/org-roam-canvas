# develop on this with:
# poetry env use 3.11
# poetry install
# poetry run uvicorn main:app --reload

# https://emacs.stackexchange.com/questions/28665/print-unquoted-output-to-stdout-from-emacsclient

from datetime import datetime
from typing import List, Optional
from pathlib import Path

from fastapi import APIRouter, Depends, FastAPI, HTTPException
from sqlalchemy import select, Column, DateTime
from sqlalchemy.ext.asyncio import create_async_engine
from sqlmodel import Relationship, create_engine, Field, Session, SQLModel
from sqlmodel.ext.asyncio.session import AsyncSession

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
    # this can be markdown, or a link, or org-id
    contents: str
    x: int
    y: int
    width: int
    height: int
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
    contents: Optional[str] = None
    x: Optional[int] = None
    y: Optional[int] = None
    width: Optional[int] = None
    height: Optional[int] = None


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

# --------------------------------------------------------------

engine = create_async_engine(f"sqlite+aiosqlite:///blap.db", echo=False, connect_args={"check_same_thread": False})

app = FastAPI()

@app.on_event("startup")
async def on_startup():
    print("ASYNC startup")
    async with engine.begin() as conn:
        await conn.run_sync(SQLModel.metadata.create_all)

    await add_canvas(CanvasCreate(name="My Canvas"))

router = APIRouter(prefix="/api/v1")

@router.get("/cavases", response_model=list[CanvasRead])
async def get_canvases():
    async with AsyncSession(engine) as session:
        result = await session.execute(select(Canvas))
        canvases = result.scalars().all()
        return canvases


@router.post("/canvases")
async def add_canvas(canvas: CanvasCreate):
    """Add a single canvas to the database.
    """
    db_canvas = Canvas.from_orm(canvas)
    async with AsyncSession(engine) as session:
        session.add(db_canvas)
        await session.commit()
        await session.refresh(db_canvas)
        return db_canvas

@router.get("/canvases/{canvas_id}", response_model=CanvasRead)
async def get_canvas(canvas_id: int):
    """Retrieve a single canvas from the database.
    """
    async with AsyncSession(engine) as session:
        canvas = await session.get(Canvas, canvas_id)
        if canvas is None:
            raise HTTPException(status_code=404, detail=f"canvas with id {canvas_id} not found")
        return canvas
    
# endpoint to update a canvas
@router.put("/canvases/{canvas_id}", response_model=CanvasRead)
async def update_canvas(canvas_id: int, canvas: CanvasUpdate):
    async with AsyncSession(engine) as session:
        db_canvas = await session.get(Canvas, canvas_id)
        if db_canvas is None:
            raise HTTPException(status_code=404, detail=f"canvas with id {canvas_id} not found")
        canvas_data = canvas.dict(exclude_unset=True)
        for key, value in canvas_data.items():
            setattr(db_canvas, key, value)

        # copilot omitted this one pretty critical line
        await session.add(db_canvas)
        await session.commit()
        await session.refresh(db_canvas)
        return db_canvas
    
# --------------------------------------------------------------
# path functions for all the nodes

@router.get("/canvases/{canvas_id}/nodes", response_model=list[NodeRead])
async def get_nodes(canvas_id: int):
    async with AsyncSession(engine) as session:
        result = await session.execute(select(Node).filter(Node.canvas_id == canvas_id))
        nodes = result.scalars().all()
        return nodes
    
@router.post("/canvases/{canvas_id}/nodes")
async def add_node(canvas_id: int, node: NodeCreate):
    """Add a node to the canvas.
    """
    db_node = Node.from_orm(node)
    async with AsyncSession(engine) as session:
        # get canvas and associate with new node (copilot omitted this association)
        # but maybe that's not necessary?
        #canvas = await session.get(Canvas, canvas_id)
        #db_node.canvas_id = canvas.id
        session.add(db_node)
        await session.commit()
        await session.refresh(db_node)
        return db_node
    
@router.get("/nodes/{node_id}", response_model=NodeRead)
async def get_node(node_id: int):
    """Retrieve a node by ID.
    """
    async with AsyncSession(engine) as session:
        node = await session.get(Node, node_id)
        if node is None:
            raise HTTPException(status_code=404, detail=f"node with id {node_id} not found")
        return node
    
@router.put("/nodes/{node_id}", response_model=NodeRead)
async def update_node(node_id: int, node: NodeUpdate):
    """Update a node by ID.
    """
    async with AsyncSession(engine) as session:
        db_node = await session.get(Node, node_id)
        if db_node is None:
            raise HTTPException(status_code=404, detail=f"node with id {node_id} not found")
        node_data = node.dict(exclude_unset=True)
        for key, value in node_data.items():
            setattr(db_node, key, value)
        await session.add(db_node)
        await session.commit()
        await session.refresh(db_node)
        return db_node
    
# --------------------------------------------------------------
# path functions for all the edges

@router.get("/canvases/{canvas_id}/edges", response_model=list[EdgeRead])
async def get_edges(canvas_id: int):
    async with AsyncSession(engine) as session:
        result = await session.execute(select(Edge).filter(Edge.canvas_id == canvas_id))
        edges = result.scalars().all()
        return edges


@router.post("/canvases/{canvas_id}/edges")
async def add_edge(canvas_id: int, edge: EdgeCreate):
    """Add an edge to the canvas.
    """
    db_edge = Edge.from_orm(edge)
    async with AsyncSession(engine) as session:
        # get canvas and associate with new edge (copilot omitted this association)
        canvas = await session.get(Canvas, canvas_id)
        db_edge.canvas_id = canvas.id
        session.add(db_edge)
        await session.commit()
        await session.refresh(db_edge)
        return db_edge

@router.get("/edges/{edge_id}", response_model=EdgeRead)
async def get_edge(edge_id: int):
    """Retrieve an edge by ID.
    """
    async with AsyncSession(engine) as session:
        edge = await session.get(Edge, edge_id)
        if edge is None:
            raise HTTPException(status_code=404, detail=f"edge with id {edge_id} not found")
        return edge
    
@router.put("/edges/{edge_id}", response_model=EdgeRead)
async def update_edge(edge_id: int, edge: EdgeUpdate):
    """Update an edge by ID.
    """
    async with AsyncSession(engine) as session:
        db_edge = await session.get(Edge, edge_id)
        if db_edge is None:
            raise HTTPException(status_code=404, detail=f"edge with id {edge_id} not found")
        edge_data = edge.dict(exclude_unset=True)
        for key, value in edge_data.items():
            setattr(db_edge, key, value)
        await session.add(db_edge)
        await session.commit()
        await session.refresh(db_edge)
        return db_edge

# have to include router after all the decorators are defined
app.include_router(router)
