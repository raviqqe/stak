import { atom } from "nanostores";

export const $source = atom(
  `
(import (scheme write))

(display "Hello, world!")
  `.trim(),
);

export const $output = atom("");

export const initializeWorker = (): Worker => {
  const worker = new Worker("./demo-worker.ts", { type: "module" });

  $source.subscribe((source) => worker.postMessage(source));
  worker.addEventListener("message", (event) => $output.set(event.data));

  return worker;
};
