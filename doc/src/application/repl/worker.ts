import init, { repl } from "@raviqqe/stak";

await init({});

const input = [];

addEventListener("message", (event) => {
  input.push(event.data);
});
const foo = {
  readInput: () => {},
  writeOutput: (byte: number) => postMessage(new Uint8Array([byte])),
};
await repl(2 ** 22);
