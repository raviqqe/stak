import Worker from "./compile/worker.js?worker";
import { runWorker } from "./run-worker.js";

export const compile = async (
  source: string,
): Promise<Uint8Array<ArrayBuffer>> => runWorker(() => new Worker(), source);
