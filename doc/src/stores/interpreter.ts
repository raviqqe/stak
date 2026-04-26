import { computedAsync } from "@nanostores/async";
import { atom } from "nanostores";
import { run as runProgram } from "../application/run.js";

export const source = atom(
  `
(import (scheme base) (scheme write))

(define (fibonacci x)
  (if (< x 2)
     x
     (+
        (fibonacci (- x 1))
        (fibonacci (- x 2)))))

(display "Answer: ")
(write (fibonacci 10))
(newline)
  `.trim(),
);

export const output = computedAsync([source], async (source) =>
  new TextDecoder().decode(await runProgram(source)),
);
