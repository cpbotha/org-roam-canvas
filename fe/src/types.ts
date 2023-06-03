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
