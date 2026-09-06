export const runWorker = async <T, S>(
  createWorker: () => Worker,
  input: T,
): Promise<S> => {
  const worker = createWorker();

  const promise = new Promise<S | Error>((resolve) =>
    worker.addEventListener("message", (event: MessageEvent<S | Error>) =>
      resolve(event.data),
    ),
  );

  worker.postMessage(input);
  const result = await promise;
  worker.terminate();

  if (result instanceof Error) {
    throw result;
  }

  return result;
};

export const runStreamWorker = <T, S>(
  createWorker: () => Worker,
  input: ReadableStream<T>,
): ReadableStream<S> => {
  const worker = createWorker();

  const output = new ReadableStream<S>({
    start: (controller) => {
      worker.addEventListener("message", (event: MessageEvent<S>) =>
        controller.enqueue(event.data),
      );
    },
  });

  void (async () => {
    for await (const message of input) {
      worker.postMessage(message);
    }
  })();

  return output;
};
