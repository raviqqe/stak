import { runWorker } from "./run-worker.js";
import Worker from "./run/worker.js?worker";

export const run = async (source: string): Promise<Uint8Array<ArrayBuffer>> =>
  runWorker(() => new Worker(), source);
