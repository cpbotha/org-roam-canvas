import { useState } from "react";

import { useQuery } from "@tanstack/react-query";

import axios from "axios";
import { INode, Node } from "./Node";

function getNodes(canvasId: number): Promise<INode[]> {
  return axios.get(`/api/canvases/${canvasId}/nodes`).then((res) => res.data);
}

export function Canvas(props: { canvasId: number }) {
  const {
    isLoading,
    error,
    data: nodes,
    isFetching,
  } = useQuery({
    queryKey: ["nodes", props.canvasId],
    queryFn: () => getNodes(props.canvasId),
  });

  if (isLoading) return "Loading...";

  if (error) {
    const msg = error instanceof Error ? error.message : "Weird type";
    return "Unable to load canvases: " + msg;
  }

  return <>{nodes && nodes.map((node: INode) => <Node node={node} />)}</>;

  //data.forEach((node: any) => {
}
