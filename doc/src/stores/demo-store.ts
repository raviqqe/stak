import { atom, computed } from "nanostores";
import CompilerWorker from "./compiler-worker.js?worker";
import InterpreterWorker from "./interpreter-worker.js?worker";
import { sleep } from "@raviqqe/loscore/async";

const workerInitializationDelay = 500;

export const sourceStore = atom(
  `
(import (scheme write))

(display "Hello, world!")
  `.trim(),
);

const bytecodeStore = atom<Uint8Array | null>(new Uint8Array());
export const compilingStore = computed(bytecodeStore, (output) => !output);

export const outputStore = atom<string | null>("");
export const interpretingStore = computed(
  outputStore,
  (output) => output === null,
);

export const compile = async (): Promise<void> => {
  bytecodeStore.set(null);
  bytecodeStore.set(
    await runWorker(() => new CompilerWorker(), sourceStore.get()),
  );
};

export const interpret = async (): Promise<void> => {
  outputStore.set(null);
  outputStore.set(
    await runWorker(() => new InterpreterWorker(), bytecodeStore.get()),
  );
};

const runWorker = async <T, S>(
  createWorker: () => Worker,
  input: T,
): Promise<S> => {
  const worker = createWorker();
  await sleep(workerInitializationDelay);

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
