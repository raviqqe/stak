import init, { compile } from "@raviqqe/stak";

await init();

addEventListener("message", (event: MessageEvent<string>) =>
  postMessage(compile(event.data)),
);
