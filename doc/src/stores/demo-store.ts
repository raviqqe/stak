import { atom, computed } from "nanostores";
import { runWorker } from "../application/run-worker.js";
import CompilerWorker from "./compiler-worker.js?worker";
import InterpreterWorker from "./interpreter-worker.js?worker";

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
