import { createEffect, onMount, type JSX } from "solid-js";
import * as xterm from "@xterm/xterm";

interface Props {
  input: WritableStream<Uint8Array>;
  output: ReadableStream<Uint8Array>;
}

export const Terminal = (props: Props): JSX.Element => {
  const terminal = new xterm.Terminal();
  let element: HTMLDivElement | null = null;

  onMount(() => {
    if (element) {
      terminal.open(element);
    }
  });

  createEffect(() => {
    void (async (input: WritableStream<Uint8Array>) => {
      const stream = new TextEncoderStream();

      terminal.onData((data) => stream.writable.getWriter().write(data));

      await stream.readable.pipeTo(input);
    })(props.input);
  });

  createEffect(() => {
    void (async (output: ReadableStream<Uint8Array>) => {
      for await (const data of output.pipeThrough(new TextDecoderStream())) {
        terminal.write(data);
      }
    })(props.output);
  });

  return (
    <div
      ref={(value) => {
        element = value;
      }}
    />
  );
};
