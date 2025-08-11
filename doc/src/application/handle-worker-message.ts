import type { Result } from "./result";

export const handleWorkerMessage = <T, S>(
  init: () => Promise<unknown>,
  handle: (input: T) => S,
): void => {
  const promise = init();

  addEventListener("message", async (event: MessageEvent<T>) => {
    await promise;

    let result: Result<S>;

    try {
      result = { value: handle(event.data) };
    } catch (error) {
      result = { error: (error as Error).message };
    }

    postMessage(result);
  });
};
