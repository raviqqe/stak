import { compile, run } from "@raviqqe/stak";
import { atom, computed } from "nanostores";

export const $source = atom(
  `
(import (scheme write))

(display "Hello, world!")
  `.trim(),
);

export const $output = computed($source, (source) =>
  new TextDecoder().decode(
    run(compile(source), new Uint8Array(0), Math.pow(2, 20)),
  ),
);
