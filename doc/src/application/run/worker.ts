import init, { run } from "@raviqqe/stak";
import { handleWorkerMessage } from "../handle-worker-message";

const heapSize: number = Math.pow(2, 20);

handleWorkerMessage(init, (input: string) =>
  run(input, new Uint8Array(), heapSize),
);
