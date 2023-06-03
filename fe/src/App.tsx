import { useState } from "react";

import { useQuery } from "@tanstack/react-query";

import axios from "axios";

import reactLogo from "./assets/react.svg";
import viteLogo from "/vite.svg";
import "./App.css";

import { Canvas } from "./Canvas";
import Embed from "react-embed";

function getCanvases() {
  return axios.get("/api/canvases").then((res) => res.data);
}

function App(): JSX.Element {
  // TODO: fix initial canvas selection later
  const [canvasId, setCanvasId] = useState(1);
  const [newOrgNodeId, setNewOrgNodeId] = useState<string | null>(null);

  async function addOrgNode() {
    const ret = await axios.get("/api/or-node-select");
    if (ret.data?.id) {
      setNewOrgNodeId(ret.data.id);
    }
  }

  const { isLoading, error, data, isFetching } = useQuery({
    queryKey: ["canvases"],
    queryFn: getCanvases,
  });

  if (isLoading) return "Loading...";

  if (error) {
    if (error instanceof Error) {
      return "Unable to load canvases: " + error.message;
    } else {
      return "Unable to fetch canvases";
    }
  }

  return (
    <>
      <div>paradigm desktop | tabletops8 | org-roam-canvas</div>
      <div id="top-level">
        {/* need preserve-3d so that all child nodes are transformed relative to the parent,
              only one level deep! */}
        <div
          id="viewportContainer"
          style={{ perspective: "600px", transformStyle: "preserve-3d" }}
        >
          {/* need transform-origin at left top so that scale keeps viewport at right place!
                  use CSS to set size of this viewport to extra large
                  */}
          <div
            id="viewport"
            style={{
              transformStyle: "preserve-3d",
              transformOrigin: "0 0",
              transform: "scale(1, 1)",
            }}
          >
            <Canvas
              canvasId={canvasId}
              newOrgNodeId={newOrgNodeId}
              setNewOrgNodeId={setNewOrgNodeId}
            />
            {/* need position absolute to put all of these divs at the same place
                      then transforms will all be relative to that --> */}
          </div>
          <button
            style={{ position: "absolute", top: "1em", left: "1em" }}
            onClick={addOrgNode}
          >
            org
          </button>
        </div>
      </div>
    </>
  );
}

export default App;
