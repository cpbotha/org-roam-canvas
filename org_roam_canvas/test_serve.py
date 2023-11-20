import subprocess

import pytest


from fastapi.testclient import TestClient
from serve import app, engine, clean_emacs_string_output

client = TestClient(app)


# test create edge with invalid node_from_id and node_to_id
# note: copilot was unable to come up with anything here really
def test_create_edge_with_invalid_node_from_id_and_node_to_id():
    # first setup test database
    response = client.post(
        "/canvases/1/edges", json={"node_from_id": 1, "node_to_id": 1}
    )
    assert engine == "bleh"
    print(engine)


EL = """(format "hello\nworld")"""

# EL = "'(42 45)"


def test_emacsclient_newlines():
    # ls -l returns a string with actual newlines
    # ls_out = subprocess.run(["ls", "-l"], text=True, capture_output=True)
    # emacs returns a string with literal "\n"
    # not only that, but it returns escaped double quotes inside the string
    e_out = subprocess.run(
        ["emacsclient", "--eval", EL], text=True, capture_output=True
    )
    assert "hello\nworld" == e_out.stdout
    # assert "hello\nworld" == clean_emacs_string_output(e_out.stdout)


# def test_get_or_node_details():
