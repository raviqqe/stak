import { atom } from "nanostores";
import Worker from "./demo-worker.js?worker";

export const $source = atom(
  `
(import (scheme write))

(display "Hello, world!")
  `.trim(),
);

export const $bytecodes = atom<Uint8Array | null>(null);

export const $output = atom("");

export const initializeCompilerWorker = (): Worker => {
  const worker = new Worker();

  $source.subscribe((source) => worker.postMessage(source));
  worker.addEventListener("message", (event: MessageEvent<string>) =>
    $output.set(event.data),
  );

  return worker;
};

export const initializeInterpreterWorker = (): Worker => {
  const worker = new Worker();

  $source.subscribe((source) => worker.postMessage(source));
  worker.addEventListener("message", (event: MessageEvent<string>) =>
    $output.set(event.data),
  );

  return worker;
};
