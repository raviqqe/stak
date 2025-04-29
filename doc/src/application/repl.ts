import { runStreamWorker } from "../application/run-worker.js";
import Worker from "./repl/worker.js?worker";

export const runRepl = async (
  program: Uint8Array,
  input: Uint8Array,
): Promise<Uint8Array<ArrayBuffer>> =>
  runStreamWorker(() => new Worker(), { input, program });
