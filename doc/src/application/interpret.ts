import Worker from "./interpret/worker.js?worker";
import { runWorker } from "./run-worker.js";

export const interpret = async (
  program: Uint8Array,
  input: Uint8Array,
): Promise<Uint8Array<ArrayBuffer>> =>
  runWorker(() => new Worker(), { input, program });
