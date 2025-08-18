import Worker from "./run/worker.js?worker";
import { runWorker } from "./run-worker.js";

export const run = async (source: string): Promise<Uint8Array<ArrayBuffer>> =>
  runWorker(() => new Worker(), source);
