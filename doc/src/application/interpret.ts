import { runWorker } from "../application/run-worker.js";
import Worker from "./interpret/worker.js?worker";

export const interpret = async (program: Uint8Array): Promise<Uint8Array> =>
  runWorker(() => new Worker(), program);
