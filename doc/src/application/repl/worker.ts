import init, { repl } from "@raviqqe/stak";

const input = new ReadableStream<number>({
  start: (controller) =>
    addEventListener("message", (event: MessageEvent<Uint8Array>) => {
      for (const byte of event.data) {
        controller.enqueue(byte);
      }
    }),
});
const reader = input.getReader();

const global = self as unknown as {
  readInput: () => Promise<number>;
  writeOutput: (byte: number) => Promise<void>;
  writeError: (byte: number) => Promise<void>;
};

global.readInput = async () => {
  const result = await reader.read();

  if (result.done) {
    throw new Error("Input stream closed");
  }

  return result.value;
};
global.writeOutput = async (byte: number) =>
  postMessage(new Uint8Array([byte]));
global.writeError = global.writeOutput;

await init({});
await repl(2 ** 22);
