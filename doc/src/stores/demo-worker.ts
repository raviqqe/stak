import { compile, interpret } from "@raviqqe/stak";

addEventListener("message", (event) => {
  postMessage(
    new TextDecoder().decode(
      interpret(compile(event.data), new Uint8Array(0), Math.pow(2, 20)),
    ),
  );
});
