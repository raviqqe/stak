import init, { interpret } from "@raviqqe/stak";

const promise = init();

// eslint-disable-next-line @typescript-eslint/no-misused-promises
addEventListener("message", async (event: MessageEvent<Uint8Array>) => {
  await promise;

  postMessage(
    new TextDecoder().decode(
      interpret(event.data, new Uint8Array(0), Math.pow(2, 22)),
    ),
  );
});
