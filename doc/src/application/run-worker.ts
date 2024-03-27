export const runWorker = async <T, S>(
  createWorker: () => Worker,
  input: T,
): Promise<S> => {
  const worker = createWorker();

  const promise = new Promise<S>((resolve) =>
    worker.addEventListener("message", (event: MessageEvent<S>) =>
      resolve(event.data),
    ),
  );

  worker.postMessage(input);
  const value = await promise;
  worker.terminate();

  return value;
};
