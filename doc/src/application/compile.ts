import { runWorker } from "../application/run-worker.js";
import Worker from "./compile/worker.js?worker";

export const compile = async (
  source: string,
): Promise<Uint8Array<ArrayBuffer>> => runWorker(() => new Worker(), source);
