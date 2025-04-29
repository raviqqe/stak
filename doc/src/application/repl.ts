import { runStreamWorker } from "../application/run-worker.js";
import Worker from "./repl/worker.js?worker";

export const runRepl = async (
  input: ReadableStream<Uint8Array>,
): ReadableStream<Uint8Array> => runStreamWorker(() => new Worker(), input);
