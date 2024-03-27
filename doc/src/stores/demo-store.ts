import { atom } from "nanostores";
import CompilerWorker from "./compiler-worker.js?worker";
import InterpreterWorker from "./interpreter-worker.js?worker";

export const $source = atom(
  `
(import (scheme write))

(display "Hello, world!")
  `.trim(),
);

export const $bytecodes = atom<Uint8Array | null>(null);

export const $output = atom("");

export const initializeCompilerWorker = (): Worker => {
  const worker = new CompilerWorker();

  $source.subscribe((source) => worker.postMessage(source));
  worker.addEventListener("message", (event: MessageEvent<Uint8Array>) =>
    $bytecodes.set(event.data),
  );

  return worker;
};

export const initializeInterpreterWorker = (): Worker => {
  const worker = new InterpreterWorker();

  $bytecodes.subscribe((bytecodes) => worker.postMessage(bytecodes));
  worker.addEventListener("message", (event: MessageEvent<string>) =>
    $output.set(event.data),
  );

  return worker;
};
