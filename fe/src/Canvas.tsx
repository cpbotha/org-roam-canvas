import { useMutation, useQuery } from "@tanstack/react-query";
import axios from "axios";
import { useEffect } from "react";

import { Node } from "./Node";
import { INode } from "./types";

function getNodes(canvasId: number): Promise<INode[]> {
  return axios.get(`/api/canvases/${canvasId}/nodes`).then((res) => res.data);
}

export function Canvas(props: {
  canvasId: number;
  newOrgNodeId: string | null;
  setNewOrgNodeId: (id: string | null) => void;
}) {
  const {
    isLoading,
    error,
    data: nodes,
    isFetching,
  } = useQuery({
    queryKey: ["nodes", props.canvasId],
    queryFn: () => getNodes(props.canvasId),
  });

  const mutation = useMutation({
    mutationFn: (newNode: INode) => {
      return axios.post(`/api/canvases/${props.canvasId}/nodes`, newNode);
    },
  });

  // see https://www.orgroam.com/manual.html#org_002droam_002dprotocol
  // for the structure of the protocol links
  // TODO: think about us shortening to just org-id://node-id
  useEffect(() => {
    if (props.newOrgNodeId) {
      // use react-query to add new node to the canvas
      console.log("newOrgNodeId", props.newOrgNodeId);
      mutation.mutate({
        title: "my first note",
        link: `org-protocol://roam-node?node=${props.newOrgNodeId}`,
        x: 400,
        y: 400,
      });
      props.setNewOrgNodeId(null);
    }
  }, [props.newOrgNodeId]);

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
