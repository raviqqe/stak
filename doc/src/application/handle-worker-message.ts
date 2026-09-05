export const handleWorkerMessage = <T, S>(
  init: () => Promise<unknown>,
  handle: (input: T) => S,
): void => {
  const promise = init();

  addEventListener("message", async (event: MessageEvent<T>) => {
    await promise;

    let result: S | Error;

    try {
      result = handle(event.data);
    } catch (error) {
      result = error instanceof Error ? error : new TypeError("Not error");
    }

    postMessage(result);
  });
};
