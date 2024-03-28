import { runWorker } from "../application/run-worker.js";
import CompilerWorker from "./compile/worker.js?worker";

export const compile = async (source: string): Promise<Uint8Array> =>
  runWorker(() => new CompilerWorker(), source);
