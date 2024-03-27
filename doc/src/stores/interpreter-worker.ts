import init, { interpret } from "@raviqqe/stak";

await init();

addEventListener("message", (event: MessageEvent<Uint8Array>) =>
  postMessage(
    new TextDecoder().decode(
      interpret(event.data, new Uint8Array(0), Math.pow(2, 22)),
    ),
  ),
);
