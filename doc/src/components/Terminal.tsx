import * as xterm from "@xterm/xterm";
import { createEffect, type JSX, onMount } from "solid-js";
import "@xterm/xterm/css/xterm.css";
import { FitAddon } from "@xterm/addon-fit";
import { delay } from "es-toolkit";
import styles from "./Terminal.module.css";

interface Props {
  initialInput?: string[];
  input: WritableStream<string>;
  output: ReadableStream<string>;
}

export const Terminal = (props: Props): JSX.Element => {
  const terminal = new xterm.Terminal();
  const fitAddon = new FitAddon();
  terminal.loadAddon(fitAddon);

  let element: HTMLDivElement | null = null;

  onMount(() => {
    if (element) {
      terminal.open(element);
      fitAddon.fit();
    }
  });

  const line: string[] = [];

  createEffect(() => {
    const writer = props.input.getWriter();

    terminal.onData(async (data) => {
      if (data === "\r") {
        await writer.write([...line.splice(0), "\n"].join(""));
        terminal.write("\r\n");
      } else if (data === "\x7f") {
        if (line.length) {
          line.pop();
          terminal.write("\b \b");
        }
      } else {
        line.push(data);
        terminal.write(data);
      }
    });

    void (async (initialInput: string[]) => {
      for (const line of initialInput) {
        await delay(500);

        for (const character of line) {
          terminal.input(character);
        }

        terminal.input("\r");
      }
    })(props.initialInput ?? []);
  });

  createEffect(() => {
    void (async (output: ReadableStream<string>) => {
      for await (const data of output) {
        terminal.write(data === "\n" ? "\r\n" : data);
      }
    })(props.output);
  });

  return (
    <div
      class={styles.root}
      ref={(value) => {
        element = value;
      }}
    />
  );
};
