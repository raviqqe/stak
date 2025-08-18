import * as xterm from "@xterm/xterm";
import type { JSX } from "preact";
import "@xterm/xterm/css/xterm.css";
import { FitAddon } from "@xterm/addon-fit";
import { delay } from "es-toolkit";
import styles from "./Terminal.module.css";
import { effect, computed } from "@preact/signals";

const inputDelay = 20;
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
  const outputs = computed(() => props.output.tee());
  const terminal = new xterm.Terminal(terminalOptions);
  const fitAddon = new FitAddon();
  terminal.loadAddon(fitAddon);

  let element: HTMLDivElement | null = null;

  effect(() => {
    if (!element) {
      return;
    }

    terminal.open(element);
    fitAddon.fit();

    element?.addEventListener("resize", () => fitAddon.fit());
  });

  const line: string[] = [];

  effect(() => {
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
        line.push(...data);
        terminal.write(data);
      }
    });
  });

  effect(() => {
    void (async (output: ReadableStream<string>) => {
      for await (const data of output) {
        terminal.write(data === "\n" ? "\r\n" : data);
      }
    })(outputs.value[0]);
  });

  effect(() => {
    void (async (output: ReadableStream<string>) => {
      await output.values().next();

      for (const line of props.initialInput ?? []) {
        await delay(inputDelay);

        for (const character of line) {
          terminal.input(character);
        }

        terminal.input("\r");
      }
    })(outputs.value[1]);
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
