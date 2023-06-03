import axios from "axios";
import { useMutation, useQuery } from "@tanstack/react-query";

import { INode } from "./types";

const mutation = useMutation({
  mutationFn: (node: INode) => {
    return axios.put(`/api/nodes/${node.id}`, node);
  },
});
