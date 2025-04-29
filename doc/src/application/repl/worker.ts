import init, { interpret } from "@raviqqe/stak";
import { handleWorkerMessage } from "../handle-worker-message";

handleWorkerMessage(
  init,
  ({ input, program }: { input: Uint8Array; program: Uint8Array }) =>
    interpret(program, input, 2 ** 22),
);
