import { useState } from 'react'

import {
  useQuery,
} from "@tanstack/react-query";

import axios from "axios";

import reactLogo from './assets/react.svg'
import viteLogo from '/vite.svg'
import './App.css'

function App() {
  const [count, setCount] = useState(0)

  const { isLoading, error, data, isFetching } = useQuery({
    queryKey: ["canvases"],
    queryFn: () =>
      axios
        .get("/api/canvases")
        .then((res) => res.data),
  });

  if (isLoading) return "Loading...";

  if (error) {
    if (error instanceof Error) {
      return "Unable to load canvases: " + error.message;
    } else {
      return "Unable to fetch canvases"
    }
  }

  return (
    <>
      <div>
        <a href="https://vitejs.dev" target="_blank">
          <img src={viteLogo} className="logo" alt="Vite logo" />
        </a>
        <a href="https://react.dev" target="_blank">
          <img src={reactLogo} className="logo react" alt="React logo" />
        </a>
      </div>
      <h1>Vite + React</h1>
      <div className="card">
        <button onClick={() => setCount((count) => count + 1)}>
          count is {count}
        </button>
        <p>
          {data.length} canvases
        </p>
      </div>
      <p className="read-the-docs">
        Click on the Vite and React logos to learn more
      </p>
    </>
  )
}

export default App
