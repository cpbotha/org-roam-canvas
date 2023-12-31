# develop on this with:
# poetry env use 3.11
# poetry install
# poetry run uvicorn serve:app --reload

# https://emacs.stackexchange.com/questions/28665/print-unquoted-output-to-stdout-from-emacsclient

import logging
import re
import subprocess
import tempfile
from pathlib import Path
from threading import Lock

import uvicorn
from fastapi import APIRouter, FastAPI, HTTPException, Response
from fastapi.staticfiles import StaticFiles
from sqlalchemy import select
from sqlalchemy.ext.asyncio import create_async_engine
from sqlmodel import SQLModel
from sqlmodel.ext.asyncio.session import AsyncSession

from models import (
    Canvas,
    CanvasCreate,
    CanvasRead,
    CanvasUpdate,
    Edge,
    EdgeCreate,
    EdgeRead,
    EdgeUpdate,
    Node,
    NodeCreate,
    NodeRead,
    NodeUpdate,
)
from utils import rewrite_links
import utils_emacs

# --------------------------------------------------------------

engine = create_async_engine(
    f"sqlite+aiosqlite:///blap.db",
    echo=False,
    connect_args={"check_same_thread": False},
)

app = FastAPI(
    openapi_url="/api/openapi.json", docs_url="/api/docs", redoc_url="/api/redoc"
)


async def add_dummy_data():
    canvas = await add_canvas(CanvasCreate(name="My Canvas"))
    node1 = await add_node(
        canvas.id,
        NodeCreate(
            title="some title.org",
            contents="My Contents",
            x=150,
            y=100,
            width=300,
            height=300,
        ),
    )
    node2 = await add_node(
        canvas.id,
        NodeCreate(
            title="another title.org",
            contents="# More Contents",
            x=170,
            y=450,
            width=300,
            height=300,
        ),
    )
    node_website = await add_node(
        canvas.id,
        NodeCreate(
            title="random website",
            link="https://cpbotha.net/",
            x=200,
            y=800,
            width=300,
            height=300,
        ),
    )
    node_youtube = await add_node(
        canvas.id,
        NodeCreate(
            title="youtube link",
            link="https://www.youtube.com/watch?v=rWJ1tPCnVJI",
            x=550,
            y=750,
            width=300,
            height=300,
        ),
    )


@app.on_event("startup")
async def on_startup():
    print("ASYNC startup")
    async with engine.begin() as conn:
        await conn.run_sync(SQLModel.metadata.create_all)

    # await add_dummy_data()


router = APIRouter(prefix="/api")


@router.get("/canvases", response_model=list[CanvasRead])
async def get_canvases():
    async with AsyncSession(engine) as session:
        result = await session.execute(select(Canvas))
        canvases = result.scalars().all()
        return canvases


@router.post("/canvases")
async def add_canvas(canvas: CanvasCreate):
    """Add a single canvas to the database."""
    db_canvas = Canvas.from_orm(canvas)
    async with AsyncSession(engine) as session:
        session.add(db_canvas)
        await session.commit()
        await session.refresh(db_canvas)
        return db_canvas


@router.get("/canvases/{canvas_id}", response_model=CanvasRead)
async def get_canvas(canvas_id: int):
    """Retrieve a single canvas from the database."""
    async with AsyncSession(engine) as session:
        canvas = await session.get(Canvas, canvas_id)
        if canvas is None:
            raise HTTPException(
                status_code=404, detail=f"canvas with id {canvas_id} not found"
            )
        return canvas


# endpoint to update a canvas
@router.put("/canvases/{canvas_id}", response_model=CanvasRead)
async def update_canvas(canvas_id: int, canvas: CanvasUpdate):
    async with AsyncSession(engine) as session:
        db_canvas = await session.get(Canvas, canvas_id)
        if db_canvas is None:
            raise HTTPException(
                status_code=404, detail=f"canvas with id {canvas_id} not found"
            )
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
    """Add a node to the canvas."""
    db_node = Node.from_orm(node)
    db_node.canvas_id = canvas_id
    async with AsyncSession(engine) as session:
        session.add(db_node)
        await session.commit()
        await session.refresh(db_node)
        return db_node


@router.get("/nodes/{node_id}", response_model=NodeRead)
async def get_node(node_id: int):
    """Retrieve a node by ID."""
    async with AsyncSession(engine) as session:
        node = await session.get(Node, node_id)
        if node is None:
            raise HTTPException(
                status_code=404, detail=f"node with id {node_id} not found"
            )
        return node


@router.patch("/nodes/{node_id}", response_model=NodeRead)
async def update_node(node_id: int, node: NodeUpdate):
    """Update a node by ID."""
    async with AsyncSession(engine) as session:
        db_node = await session.get(Node, node_id)
        if db_node is None:
            raise HTTPException(
                status_code=404, detail=f"node with id {node_id} not found"
            )
        node_data = node.dict(exclude_unset=True)
        for key, value in node_data.items():
            setattr(db_node, key, value)

        session.add(db_node)
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
    """Add an edge to the canvas."""
    db_edge = Edge.from_orm(edge)
    db_edge.canvas_id = canvas_id
    async with AsyncSession(engine) as session:
        session.add(db_edge)
        await session.commit()
        await session.refresh(db_edge)
        return db_edge


@router.get("/edges/{edge_id}", response_model=EdgeRead)
async def get_edge(edge_id: int):
    """Retrieve an edge by ID."""
    async with AsyncSession(engine) as session:
        edge = await session.get(Edge, edge_id)
        if edge is None:
            raise HTTPException(
                status_code=404, detail=f"edge with id {edge_id} not found"
            )
        return edge


@router.put("/edges/{edge_id}", response_model=EdgeRead)
async def update_edge(edge_id: int, edge: EdgeUpdate):
    """Update an edge by ID."""
    async with AsyncSession(engine) as session:
        db_edge = await session.get(Edge, edge_id)
        if db_edge is None:
            raise HTTPException(
                status_code=404, detail=f"edge with id {edge_id} not found"
            )
        edge_data = edge.dict(exclude_unset=True)
        for key, value in edge_data.items():
            setattr(db_edge, key, value)
        await session.add(db_edge)
        await session.commit()
        await session.refresh(db_edge)
        return db_edge


@router.get("/or-node-select")
def select_node():
    return utils_emacs.select_node()


@router.get("/or-nodes/{or_node_id}")
def get_or_node_details(or_node_id: str):
    return utils_emacs.get_or_node_details(or_node_id)


# have to include router after all the decorators are defined
app.include_router(router)
app.mount("/orc-files", StaticFiles(directory="/"), name="orc-files")

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000)
