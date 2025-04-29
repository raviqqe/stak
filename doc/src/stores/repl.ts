import { atom, computed, task } from "nanostores";
import { runRepl } from "../application/repl.js";

export const input = atom<ReadableStream<Uint8Array>>(new ReadableStream());

const run = computed(input, (input) =>
  task(async () => {
    try {
      return await runRepl(input);
    } catch (error) {
      return error as Error;
    }
  }),
);

export const output = computed(run, (output) =>
  output instanceof Error ? null : new TextDecoder().decode(output),
);

export const error = computed(run, (error) =>
  error instanceof Error ? error : null,
);
