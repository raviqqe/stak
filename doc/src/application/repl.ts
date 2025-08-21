import Worker from "./repl/worker.js?worker";
import { runStreamWorker } from "./run-worker.js";

export const runRepl = (
  input: ReadableStream<Uint8Array>,
): ReadableStream<Uint8Array<ArrayBuffer>> =>
  runStreamWorker(() => new Worker(), input);
