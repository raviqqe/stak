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
    const encoder = new TextEncoder();
    const writer = props.input.getWriter();

    terminal.onData((data) => writer.write(encoder.encode(data)));
  });

  createEffect(() => {
    const decoder = new TextDecoder();

    void (async (output: ReadableStream<Uint8Array>) => {
      for await (const data of output) {
        terminal.write(decoder.decode(data));
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
