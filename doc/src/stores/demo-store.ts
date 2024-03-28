import { atom, computed } from "nanostores";
import { compile as compileProgram } from "../application/compile.js";
import { interpret as interpretProgram } from "../application/interpret.js";

export const sourceStore = atom("");

const bytecodeStore = atom<Uint8Array | null>(new Uint8Array());

export const compilingStore = computed(bytecodeStore, (output) => !output);

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

export const compile = async (): Promise<void> => {
  bytecodeStore.set(null);
  bytecodeStore.set(await compileProgram(sourceStore.get()));
};

export const interpret = async (): Promise<void> => {
  const bytecodes = bytecodeStore.get();

  if (!bytecodes) {
    return;
  }

  binaryOutputStore.set(null);

  const output = await interpretProgram(
    bytecodes,
    new TextEncoder().encode(inputStore.get()),
  );

  binaryOutputStore.set(output);
};
