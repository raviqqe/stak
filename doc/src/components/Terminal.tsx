import { createEffect, onMount, type JSX } from "solid-js";
import * as xterm from "@xterm/xterm";

interface Props {
  input: ReadableStream<Uint8Array>;
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
    const decoder = new TextDecoder();

    void (async (input: ReadableStream<Uint8Array>) => {
      for await (const data of input) {
        terminal.input(decoder.decode(data));
      }
    })(props.input);
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
