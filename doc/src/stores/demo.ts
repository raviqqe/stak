import { atom, computed } from "nanostores";
import { compile as compileProgram } from "../application/compile.js";
import { interpret as interpretProgram } from "../application/interpret.js";

export const sourceStore = atom(
  `
(import (scheme base) (scheme read) (scheme write))

(display "Hello, world!")
  `.trim(),
);

const bytecodeStore = atom<Uint8Array | null>(new Uint8Array());

export const compilingStore = computed(
  bytecodeStore,
  (output) => output === null,
);

const binaryOutputStore = atom<Uint8Array | null>(new Uint8Array());

export const inputStore = atom("");
export const outputStore = computed(binaryOutputStore, (output) =>
  output === null ? null : new TextDecoder().decode(output),
);

export const outputUrlStore = computed(binaryOutputStore, (output) =>
  output?.length ? URL.createObjectURL(new Blob([output])) : null,
);

export const interpretingStore = computed(
  outputStore,
  (output) => output === null,
);

export const compilerErrorStore = atom("");

export const interpreterErrorStore = atom("");

export const compile = async (): Promise<void> => {
  bytecodeStore.set(null);
  compilerErrorStore.set("");

  let bytecodes = new Uint8Array();

  try {
    bytecodes = await compileProgram(sourceStore.get());
  } catch (error) {
    compilerErrorStore.set((error as Error).message);
  }

  bytecodeStore.set(bytecodes);
};

export const interpret = async (): Promise<void> => {
  const bytecodes = bytecodeStore.get();

  if (!bytecodes) {
    return;
  }

  binaryOutputStore.set(null);
  interpreterErrorStore.set("");

  let output = new Uint8Array();

  try {
    output = await interpretProgram(
      bytecodes,
      new TextEncoder().encode(inputStore.get()),
    );
  } catch (error) {
    interpreterErrorStore.set((error as Error).message);
  }

  binaryOutputStore.set(output);
};
