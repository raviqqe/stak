import { runWorker } from "../application/run-worker.js";
import InterpreterWorker from "./interpret/worker.js?worker";

export const interpret = async (source: Uint8Array): Promise<Uint8Array> =>
  runWorker(() => new InterpreterWorker(), source);
