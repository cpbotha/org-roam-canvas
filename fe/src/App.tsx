import { useState } from "react";

import { useQuery } from "@tanstack/react-query";

import axios from "axios";

import reactLogo from "./assets/react.svg";
import viteLogo from "/vite.svg";
import "./App.css";

import { Canvas } from "./Canvas";

function getCanvases() {
  return axios.get("/api/canvases").then((res) => res.data);
}

function App() {
  // TODO: fix initial canvas selection later
  const [canvasId, setCanvasId] = useState(1);

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
      <div>navigation</div>
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
            <div
              className="thing"
              id="img-upload"
              style={{
                position: "absolute",
                top: "0px",
                left: "0px",
                border: "1px solid black",
                transform: "translate(300px, 300px)",
              }}
            >
              Hello dere!
            </div>
          </div>
        </div>
      </div>
    </>
  );
}

export default App;
