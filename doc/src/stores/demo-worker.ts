import { compile, run } from "@raviqqe/stak";

addEventListener("message", (event) => {
  postMessage(
    new TextDecoder().decode(
      run(compile(event.data), new Uint8Array(0), Math.pow(2, 20)),
    ),
  );
});
