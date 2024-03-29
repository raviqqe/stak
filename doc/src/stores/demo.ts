import { atom, computed } from "nanostores";
import { compile as compileProgram } from "../application/compile.js";
import { interpret as interpretProgram } from "../application/interpret.js";

export const source = atom(
  `
(import (scheme base) (scheme read) (scheme write))

(display "Hello, world!")
  `.trim(),
);

const bytecodes = atom<Uint8Array | null>(new Uint8Array());

export const compiling = computed(bytecodes, (output) => output === null);

const binaryOutput = atom<Uint8Array | null>(new Uint8Array());

export const input = atom("");
export const textOutput = computed(binaryOutput, (output) =>
  output === null ? null : new TextDecoder().decode(output),
);

export const outputUrlStore = computed(binaryOutput, (output) =>
  output?.length ? URL.createObjectURL(new Blob([output])) : null,
);

export const interpretingStore = computed(
  binaryOutput,
  (output) => output === null,
);

export const compilerError = atom("");

export const interpreterError = atom("");

export const compile = async (): Promise<void> => {
  bytecodes.set(null);
  compilerError.set("");

  let value = new Uint8Array();

  try {
    value = await compileProgram(source.get());
  } catch (error) {
    compilerError.set((error as Error).message);
  }

  bytecodes.set(value);
};

export const interpret = async (): Promise<void> => {
  const value = bytecodes.get();

  if (!value) {
    return;
  }

  binaryOutput.set(null);
  interpreterError.set("");

  let output = new Uint8Array();

  try {
    output = await interpretProgram(
      value,
      new TextEncoder().encode(input.get()),
    );
  } catch (error) {
    interpreterError.set((error as Error).message);
  }

  binaryOutput.set(output);
};
