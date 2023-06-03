// TODO: oembed, react-iframe, etc.
// react-embed is quite out of date :(

import { useQuery } from "@tanstack/react-query";
import axios from "axios";
import htmr from "htmr";
import { useState } from "react";
import Draggable from "react-draggable";
import Embed from "react-embed";
import { ResizableBox } from "react-resizable";

import "./Node.css";
import { INode } from "./types";

interface IOrgNodeDetails {
  file: string;
  title: string;
  html: string;
}

function getOrgNodeDetails(orgId: string): Promise<IOrgNodeDetails> {
  return axios.get(`/api/or-nodes/${orgId}`).then((res) => res.data);
}

function RenderOrgNode(props: { orgId: string }): JSX.Element {
  const { orgId } = props;

  const { data: orgNodeDetails } = useQuery({
    queryKey: ["or-nodes", orgId],
    queryFn: () => getOrgNodeDetails(orgId),
  });

  // todo: htmr
  if (orgNodeDetails) {
    return htmr(orgNodeDetails.html);
  }
  return <div>HELLO Org node: {orgId}</div>;
}

function RenderNodeNonEmbed(props: { node: INode }) {
  const { node } = props;

  if (node.link) {
    if (node.link.startsWith("org-protocol://roam-node?node=")) {
      // parse out the part after the =
      const orgId = node.link.split("=")[1];
      return <RenderOrgNode orgId={orgId} />;
    }

    // render iframe or oembed
    return (
      <div className="node">
        <div className="node-title">{node.title}</div>
        <div className="node-contents">{node.link}</div>
      </div>
    );
  }

  return (
    <div className="node">
      <div className="node-title">{node.title}</div>
      <div className="node-contents">{node.contents}</div>
    </div>
  );
}

export function Node(props: { node: INode }) {
  const { node } = props;

  const [width, setWidth] = useState(node.width);
  const [height, setHeight] = useState(node.height);

  // Embed only does the sites and filetypes it explicitly supports
  // for other websites, we should try a fallback
  //node.link = "https://soundcloud.com/kink/mechtaya";

  // manual transform
  // style.transform: `translate(${node.x}px, ${node.y}px)`,
  // with react-draggable, CSS transform of child is updated
  return (
    <Draggable
      defaultClassName="node"
      defaultPosition={{ x: node.x, y: node.y }}
      handle=".handle"
      cancel={".react-resizable-handle"}
    >
      <ResizableBox
        width={width}
        height={height}
        onResize={(event, { node, size, handle }) => {
          setWidth(size.width);
          setHeight(size.height);
        }}
      >
        <div
          className="node-NOT"
          style={{
            width: `${width}px`,
            height: `${height}px`,
          }}
        >
          <div className="handle" style={{ background: "lightgrey" }}>
            {node.title}
          </div>
          <div
            style={{
              overflow: "auto",
              height: "calc(100% - 2em)",
              width: "100%",
            }}
          >
            {node.link && (
              <Embed
                url={node.link}
                renderVoid={(props, state, error) => (
                  <RenderNodeNonEmbed node={node} />
                )}
              />
            )}
          </div>
        </div>
      </ResizableBox>
    </Draggable>
  );
}
