import { runStreamWorker } from "../application/run-worker.js";
import Worker from "./repl/worker.js?worker";

export const runRepl = (
  input: ReadableStream<Uint8Array>,
): ReadableStream<Uint8Array<ArrayBuffer>> =>
  runStreamWorker(() => new Worker(), input);
