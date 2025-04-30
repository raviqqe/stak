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
  // eslint-disable-next-line @typescript-eslint/naming-convention
  read_stdin: () => Promise<number>;
  // eslint-disable-next-line @typescript-eslint/naming-convention
  write_stderr: (byte: number) => Promise<void>;
  // eslint-disable-next-line @typescript-eslint/naming-convention
  write_stdout: (byte: number) => Promise<void>;
};

global.read_stdin = async () => {
  const result = await reader.read();

  if (result.done) {
    throw new Error("Input stream closed");
  }

  return result.value;
};
global.write_stdout = global.write_stderr = (byte: number) =>
  Promise.resolve(postMessage(new Uint8Array([byte])));

await init();
await repl(2 ** 22);
