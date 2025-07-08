import { createEffect, onMount, type JSX } from "solid-js";
import * as xterm from "@xterm/xterm";

interface Props {
  input: WritableStream<string>;
  output: ReadableStream<string>;
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
    const writer = props.input.getWriter();

    terminal.onData((data) => writer.write(data));
  });

  createEffect(() => {
    void (async (output: ReadableStream<string>) => {
      for await (const data of output) {
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
