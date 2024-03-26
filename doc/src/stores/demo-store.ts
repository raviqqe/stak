import { atom } from "nanostores";

const worker = new Worker("./demo-worker.ts", { type: "module" });

worker.postMessage("Hello");

export const $source = atom(
  `
(import (scheme write))

(display "Hello, world!")
  `.trim(),
);

export const $output = atom("");

worker.addEventListener("message", (event) => $output.set(event.data));
