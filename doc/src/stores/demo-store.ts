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
  const source = sourceStore.get();
  bytecodeStore.set(null);

  const worker = new CompilerWorker();
  await sleep(workerInitializationDelay);

  const promise = new Promise<Uint8Array>((resolve) =>
    worker.addEventListener("message", (event: MessageEvent<Uint8Array>) =>
      resolve(event.data),
    ),
  );
  worker.postMessage(source);
  bytecodeStore.set(await promise);

  worker.terminate();
};

export const interpret = async (): Promise<void> => {
  const bytecodes = bytecodeStore.get();

  outputStore.set(null);

  const worker = new InterpreterWorker();
  await sleep(workerInitializationDelay);

  const promise = new Promise<string>((resolve) =>
    worker.addEventListener("message", (event: MessageEvent<string>) =>
      resolve(event.data),
    ),
  );

  worker.postMessage(bytecodes);
  outputStore.set(await promise);

  worker.terminate();
};
