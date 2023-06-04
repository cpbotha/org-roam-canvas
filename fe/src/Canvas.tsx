import { useMutation, useQuery } from "@tanstack/react-query";
import axios from "axios";
import { useEffect } from "react";

import { Node } from "./Node";
import { INode } from "./types";

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
    queryKey: ["canvases", props.canvasId, "nodes"],
    queryFn: () => getNodes(props.canvasId),
  });

  const mutation = useMutation({
    mutationFn: (newNode: INode) => {
      return axios.post(`/api/canvases/${props.canvasId}/nodes`, newNode);
    },
  });

  if (isLoading) return "Loading...";

  if (error) {
    const msg = error instanceof Error ? error.message : "Weird type";
    return "Unable to load canvases: " + msg;
  }

  return (
    <>
      {nodes && nodes.map((node: INode) => <Node key={node.id} node={node} />)}
    </>
  );

  //data.forEach((node: any) => {
}
