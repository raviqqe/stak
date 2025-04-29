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

(window as unknown as { readInput: () => Promise<number> }).readInput =
  async () => {
    const result = await reader.read();

    if (result.done) {
      throw new Error("Input stream closed");
    }

    return result.value;
  };

(
  window as unknown as { writeOutput: (byte: number) => Promise<void> }
).writeOutput = async (byte: number) => postMessage(new Uint8Array([byte]));

await init({});
await repl(2 ** 22);
