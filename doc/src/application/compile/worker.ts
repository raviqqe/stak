import init, { compile } from "@raviqqe/stak";
import type { Result } from "../result";

const promise = init();

// eslint-disable-next-line @typescript-eslint/no-misused-promises
addEventListener("message", async (event: MessageEvent<string>) => {
  await promise;

  let result: Result<Uint8Array>;

  try {
    result = { value: compile(event.data) };
  } catch (error) {
    result = { error: (error as Error).message };
  }

  postMessage(result);
});
