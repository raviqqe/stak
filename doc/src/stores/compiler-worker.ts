import init, { compile } from "@raviqqe/stak";

const promise = init();

addEventListener("message", async (event: MessageEvent<string>) => {
  await promise;

  postMessage(compile(event.data));
});
