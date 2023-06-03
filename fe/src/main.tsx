import React from "react";
import ReactDOM from "react-dom/client";

import {
  QueryClient,
  QueryClientProvider,
  useQuery,
} from "@tanstack/react-query";

import App from "./App.tsx";
import "./index.css";

// set default staleTime to 20s
// https://tkdodo.eu/blog/react-query-as-a-state-manager
const queryClient = new QueryClient({
  defaultOptions: { queries: { staleTime: 1000 * 20 } },
});

ReactDOM.createRoot(document.getElementById("root") as HTMLElement).render(
  <React.StrictMode>
    <QueryClientProvider client={queryClient}>
      <App />
    </QueryClientProvider>
  </React.StrictMode>
);
