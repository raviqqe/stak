import { atom, computed } from "nanostores";
import { compile as compileProgram } from "../application/compile.js";
import { interpret as interpretProgram } from "../application/interpret.js";

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
  bytecodeStore.set(await compileProgram(sourceStore.get()));
};

export const interpret = async (): Promise<void> => {
  const bytecodes = bytecodeStore.get();

  if (!bytecodes) {
    return;
  }

  outputStore.set(null);

  const output = await interpretProgram(bytecodes);

  outputStore.set(new TextDecoder().decode(output));
};
