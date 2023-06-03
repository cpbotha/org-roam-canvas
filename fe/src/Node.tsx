// TODO: oembed, react-iframe, etc.
// react-embed is quite out of date :(

import Embed, { route } from "react-embed";

import { useMutation, useQuery } from "@tanstack/react-query";
import axios from "axios";

import htmr from "htmr";
import "./Node.css";

export interface INode {
  title: string;
  contents?: string;
  link?: string;
  x: number;
  y: number;
  width?: number;
  height?: number;
  colour?: string | null;
  canvas_id?: number | null;
  id?: number | null;
  created_at?: Date | null;
}

interface IOrgNodeDetails {
  file: string;
  title: string;
  html: string;
}

function getOrgNodeDetails(orgId: string): Promise<IOrgNodeDetails> {
  return axios.get(`/api/or-nodes/${orgId}`).then((res) => res.data);
}

function RenderOrgNode(props: { orgId: string }) {
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

  // Embed only does the sites and filetypes it explicitly supports
  // for other websites, we should try a fallback
  //node.link = "https://soundcloud.com/kink/mechtaya";

  return (
    <div
      className="node"
      style={{
        width: `${node.width}px`,
        height: `${node.height}px`,
        transform: `translate(${node.x}px, ${node.y}px)`,
      }}
    >
      {node.title}
      {node.link && (
        <Embed
          url={node.link}
          renderVoid={(props, state, error) => (
            <RenderNodeNonEmbed node={node} />
          )}
        />
      )}
    </div>
  );
}
