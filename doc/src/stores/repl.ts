import { atom, computed } from "nanostores";
import { runRepl } from "../application/repl.js";

export const input = atom<ReadableStream<Uint8Array>>(new ReadableStream());

export const output = computed(input, (input) => runRepl(input));
