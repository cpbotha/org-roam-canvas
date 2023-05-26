import pytest
from fastapi.testclient import TestClient
from .serve import app

client = TestClient(app)

# test create edge with invalid node_from_id and node_to_id
# note: copilot was unable to come up with anything here really
def test_create_edge_with_invalid_node_from_id_and_node_to_id():
    # first setup test database
    response = client.post("/canvases/1/edges", json={"node_from_id": 1, "node_to_id": 1})
