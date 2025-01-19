import { atom, computed } from "nanostores";
import { run as runProgram } from "../application/run.js";

export const source = atom(
  `
(import (scheme base) (scheme write))

(display "Hello, world!\n")
  `.trim(),
);

export const output = computed(source, async (source) => {
  try {
    return await runProgram(source);
  } catch (error) {
    return error;
  }
});
