import "@xterm/xterm/css/xterm.css";
import { useSignalEffect } from "@preact/signals";
import { useSignalRef } from "@preact/signals/utils";
import { FitAddon } from "@xterm/addon-fit";
import * as xterm from "@xterm/xterm";
import { delay } from "es-toolkit";
import type { FunctionComponent } from "preact";
import styles from "./Terminal.module.css";

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

export const Terminal: FunctionComponent<Props> = ({
  id,
  input,
  output,
  initialInput,
}) => {
  const terminal = new xterm.Terminal(terminalOptions);
  const outputs = output.tee();

  const ref = useSignalRef<HTMLDivElement | null>(null);

  useSignalEffect(() => {
    if (!ref.current) {
      return;
    }

    const fitAddon = new FitAddon();

    terminal.loadAddon(fitAddon);
    terminal.open(ref.current);
    fitAddon.fit();

    ref.current.addEventListener("resize", () => fitAddon.fit());
  });

  const line: string[] = [];

  useSignalEffect(() => {
    const writer = input.getWriter();

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

  useSignalEffect(() => {
    void (async () => {
      for await (const data of outputs[0]) {
        terminal.write(data === "\n" ? "\r\n" : data);
      }
    })();

    void (async () => {
      await outputs[1].values().next();

      for (const line of initialInput ?? []) {
        await delay(inputDelay);

        for (const character of line) {
          terminal.input(character);
        }

        terminal.input("\r");
      }
    })();
  });

  return <div class={styles.root} id={id} ref={ref} />;
};
