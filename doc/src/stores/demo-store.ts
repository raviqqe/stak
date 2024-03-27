import { atom, computed } from "nanostores";
import CompilerWorker from "./compiler-worker.js?worker";
import InterpreterWorker from "./interpreter-worker.js?worker";

export const $source = atom(
  `
(import (scheme write))

(display "Hello, world!")
  `.trim(),
);

const $compilerInput = atom("");
const $compilerOutput = atom<Uint8Array | null>(new Uint8Array());
export const $compiling = computed($compilerOutput, (output) => !output);

const $interpreterInput = atom<Uint8Array | null>(null);
export const $interpreterOutput = atom("");
export const $interpreting = computed($interpreterOutput, (output) => !output);

export const initializeCompilerWorker = (): Worker => {
  const worker = new CompilerWorker();

  $compilerInput.subscribe((source) => {
    worker.postMessage(source);
    $compilerOutput.set(null);
  });
  worker.addEventListener("message", (event: MessageEvent<Uint8Array>) =>
    $compilerOutput.set(event.data),
  );

  return worker;
};

export const initializeInterpreterWorker = (): Worker => {
  const worker = new InterpreterWorker();

  $interpreterInput.subscribe((bytecodes) => {
    if (bytecodes) {
      worker.postMessage(bytecodes);
      $interpreterOutput.set("");
    }
  });
  worker.addEventListener("message", (event: MessageEvent<string>) =>
    $interpreterOutput.set(event.data),
  );

  return worker;
};

export const compile = (): void => $compilerInput.set($source.get());

export const interpret = (): void =>
  $interpreterInput.set($compilerOutput.get());
