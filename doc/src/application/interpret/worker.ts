import init, { interpret } from "@raviqqe/stak";
import { handleWorkerMessage } from "../handle-worker-message";

handleWorkerMessage(
  init,
  ({ program, input }: { program: Uint8Array; input: Uint8Array }) =>
    interpret(program, input, 2 ** 22),
);
