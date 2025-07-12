import * as xterm from "@xterm/xterm";
import { createEffect, createMemo, type JSX, onMount } from "solid-js";
import "@xterm/xterm/css/xterm.css";
import { createResizeObserver } from "@solid-primitives/resize-observer";
import { FitAddon } from "@xterm/addon-fit";
import { delay } from "es-toolkit";
import styles from "./Terminal.module.css";

const inputDelay = 100;
const terminalOptions: xterm.ITerminalOptions = {
  lineHeight: 1.1,
  tabStopWidth: 2,
};

interface Props {
  id?: string;
  initialInput?: string[];
  input: WritableStream<string>;
  output: ReadableStream<string>;
}

export const Terminal = (props: Props): JSX.Element => {
  const outputs = createMemo(() => props.output.tee());
  const terminal = new xterm.Terminal(terminalOptions);
  const fitAddon = new FitAddon();
  terminal.loadAddon(fitAddon);

  let element: HTMLDivElement | null = null;

  onMount(() => {
    if (!element) {
      return;
    }

    terminal.open(element);
    fitAddon.fit();
    createResizeObserver(element, () => fitAddon.fit());
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
  });

  createEffect(() => {
    void (async (output: ReadableStream<string>) => {
      for await (const data of output) {
        terminal.write(data === "\n" ? "\r\n" : data);
      }
    })(outputs()[0]);
  });

  createEffect(() => {
    void (async (output: ReadableStream<string>) => {
      await output.values().next();

      for (const line of props.initialInput ?? []) {
        await delay(inputDelay);

        for (const character of line) {
          terminal.input(character);
        }

        terminal.input("\r");
      }
    })(outputs()[1]);
  });

  return (
    <div
      class={styles.root}
      id={props.id}
      ref={(value) => {
        element = value;
      }}
    />
  );
};
