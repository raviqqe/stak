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
  read_stdin: () => Promise<number>;
  write_stdout: (byte: number) => Promise<void>;
  write_stderr: (byte: number) => Promise<void>;
};

global.read_stdin = async () => {
  const result = await reader.read();

  if (result.done) {
    throw new Error("Input stream closed");
  }

  return result.value;
};
global.write_stdout = async (byte: number) =>
  postMessage(new Uint8Array([byte]));
global.write_stderr = global.write_stdout;

await init({});
await repl(2 ** 22);
