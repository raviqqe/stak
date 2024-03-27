import init, { interpret } from "@raviqqe/stak";

const promise = init();

addEventListener("message", async (event: MessageEvent<Uint8Array>) => {
  await promise;

  postMessage(
    new TextDecoder().decode(
      interpret(event.data, new Uint8Array(0), Math.pow(2, 22)),
    ),
  );
});
