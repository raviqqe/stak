import { runWorker } from "../application/run-worker.js";
import Worker from "./compile/worker.js?worker";
import type { Result } from "./result.js";

export const compile = async (source: string): Promise<Result<Uint8Array>> =>
  runWorker(() => new Worker(), source);
