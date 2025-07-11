import "@xterm/xterm/css/xterm.css";
import { createEffect, onMount, type JSX } from "solid-js";
import * as xterm from "@xterm/xterm";
import { Readline } from "xterm-readline";

interface Props {
  input: WritableStream<string>;
  output: ReadableStream<string>;
}

export const Terminal = (props: Props): JSX.Element => {
  const terminal = new xterm.Terminal();
  terminal.loadAddon(new Readline());
  let element: HTMLDivElement | null = null;

  onMount(() => {
    if (element) {
      terminal.open(element);
    }
  });

  createEffect(() => {
    const writer = props.input.getWriter();

    terminal.onData(async (data) => {
      if (data == "\r") {
        await writer.write("\n");
        terminal.write("\r\n");
      } else {
        await writer.write(data);
        terminal.write(data);
      }
    });
  });

  createEffect(() => {
    void (async (output: ReadableStream<string>) => {
      for await (const data of output) {
        terminal.write(data == "\n" ? "\r\n" : data);
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
