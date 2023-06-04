import { useQuery } from "@tanstack/react-query";
import axios from "axios";
import { useState } from "react";

import "./App.css";
import { Canvas } from "./Canvas";
import { useAddNode } from "./hooks";
import { ICanvas } from "./types";

function getCanvases(): Promise<ICanvas[]> {
  return axios.get("/api/canvases").then((res) => res.data);
}

function basename(path: string): string {
  // Remove trailing slashes and backslashes
  const trimmedPath = path.replace(/[\\/]+$/, "");

  // Get the last segment after splitting by slashes
  const segments = trimmedPath.split(/[\\/]/);
  let filename = segments[segments.length - 1];

  return filename;
}

function App(): JSX.Element {
  // TODO: fix initial canvas selection later
  const [canvasId, setCanvasId] = useState(1);

  const { mutate: addNode } = useAddNode(canvasId);

  async function addOrgNode() {
    const ret = await axios.get("/api/or-node-select");
    if (ret.data?.id) {
      // see https://www.orgroam.com/manual.html#org_002droam_002dprotocol
      addNode({
        title: basename(ret.data.file),
        link: `org-protocol://roam-node?node=${ret.data.id}`,
        x: 400,
        y: 400,
      });
    }
  }

  const {
    isLoading,
    error,
    data: canvases,
    isFetching,
  } = useQuery({
    queryKey: ["canvases"],
    queryFn: getCanvases,
  });

  if (isLoading) return <div>Loading...</div>;

  if (error) {
    const msg =
      error instanceof Error
        ? "Unable to load canvases: " + error.message
        : "Unable to fetch canvases";
    return <div>{msg}</div>;
  }

  const canvas = canvases?.find((canvas: any) => canvas.id === canvasId);
  if (!canvas) {
    return <div>Unable to find canvas with id {canvasId}</div>;
  }

  return (
    <>
      <div>org-roam-canvas | {canvas.name}</div>
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
            <Canvas canvasId={canvasId} />
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
