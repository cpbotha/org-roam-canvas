import Embed, { route } from "react-embed";

import "./Node.css";

export interface INode {
  title: string;
  contents?: string;
  link?: string;
  x: number;
  y: number;
  width: number;
  height: number;
  colour?: string | null;
  canvas_id?: number | null;
  id?: number | null;
  created_at?: Date | null;
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
          renderVoid={(props, state, error) => <div>{props.url}</div>}
        />
      )}
    </div>
  );
}
