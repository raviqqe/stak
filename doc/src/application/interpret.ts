import { runWorker } from "../application/run-worker.js";
import Worker from "./interpret/worker.js?worker";

export const interpret = async (source: Uint8Array): Promise<Uint8Array> =>
  runWorker(() => new Worker(), source);
