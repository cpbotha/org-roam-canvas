import axios from "axios";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";

import { INode } from "./types";

//
export const useUpdateNode = (nodeId: number) => {
  const queryClient = useQueryClient();

  // https://tanstack.com/query/latest/docs/react/guides/mutations
  return useMutation({
    mutationFn: (node: Partial<INode>) =>
      axios.patch(`/api/nodes/${nodeId}`, node).then((res) => res.data),
    onSuccess: (updatedNode) => {
      // https://tkdodo.eu/blog/mastering-mutations-in-react-query#direct-updates
      queryClient.setQueryData(["nodes", nodeId], updatedNode);
    },
  });
};
