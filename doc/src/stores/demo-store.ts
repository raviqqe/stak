import { atom } from "nanostores";
import Worker from "./demo-worker.js?worker";

export const $source = atom(
  `
(import (scheme write))

(display "Hello, world!")
  `.trim(),
);

export const $output = atom<string | null>(null);

export const initializeWorker = (): Worker => {
  const worker = new Worker();

  $source.subscribe((source) => worker.postMessage(source));
  worker.addEventListener("message", (event: MessageEvent<string>) =>
    $output.set(event.data),
  );

  return worker;
};
