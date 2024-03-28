import init, { interpret } from "@raviqqe/stak";

const promise = init();

addEventListener(
  "message",
  // eslint-disable-next-line @typescript-eslint/no-misused-promises
  async (event: MessageEvent<{ program: Uint8Array; input: Uint8Array }>) => {
    await promise;

    let output = new Uint8Array(0);

    try {
      output = interpret(event.data.program, event.data.input, Math.pow(2, 22));
    } catch (error) {
      // TODO Handle an error.
    }

    postMessage(output);
  },
);
