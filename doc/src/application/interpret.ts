import { runWorker } from "../application/run-worker.js";
import Worker from "./interpret/worker.js?worker";
import type { Result } from "./result.js";

export const interpret = async (
  program: Uint8Array,
  input: Uint8Array,
): Promise<Result<Uint8Array>> =>
  runWorker(() => new Worker(), { input, program });
