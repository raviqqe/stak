import type { Result } from "./result";

export const runWorker = async <T, S>(
  createWorker: () => Worker,
  input: T,
): Promise<S> => {
  const worker = createWorker();

  const promise = new Promise<Result<S>>((resolve) =>
    worker.addEventListener("message", (event: MessageEvent<Result<S>>) =>
      resolve(event.data),
    ),
  );

  worker.postMessage(input);
  const result = await promise;
  worker.terminate();

  if (typeof result.error === "string") {
    throw new Error(result.error);
  }

  return result.value;
};

export const runStreamWorker = async <T, S>(
  createWorker: () => Worker,
  input: ReadableStream<T>,
): Promise<ReadableStream<S>> => {
  const worker = createWorker();

  void (async () => {
    for await (const message of input) {
      worker.postMessage(message);
    }
  })();

  const output = new ReadableStream({
    start: (controller) => {
      worker.addEventListener("message", (event: MessageEvent<Result<S>>) =>
        controller.enqueue(event.data),
      );
    },
  });

  return output;
};
