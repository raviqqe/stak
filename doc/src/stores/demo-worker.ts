import init, { compile, interpret } from "@raviqqe/stak";

await init();

addEventListener("message", (event: MessageEvent<string>) => {
  postMessage(
    new TextDecoder().decode(
      interpret(compile(event.data), new Uint8Array(0), Math.pow(2, 20)),
    ),
  );
});
