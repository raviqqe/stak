import init, { compile } from "@raviqqe/stak";

const promise = init();

// eslint-disable-next-line @typescript-eslint/no-misused-promises
addEventListener("message", async (event: MessageEvent<string>) => {
  await promise;

  postMessage(compile(event.data));
});
