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
    </div>
  );
}
