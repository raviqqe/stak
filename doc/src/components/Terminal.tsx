import "@xterm/xterm/css/xterm.css";
import * as xterm from "@xterm/xterm";
import { createRef, type FunctionComponent } from "preact";
import { FitAddon } from "@xterm/addon-fit";
import { delay } from "es-toolkit";
import styles from "./Terminal.module.css";
import { useEffect, useMemo } from "preact/hooks";

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
  const terminal = useMemo(() => new xterm.Terminal(terminalOptions), []);
  const outputs = useMemo(() => output.tee(), [output]);

  const ref = createRef();

  useEffect(() => {
    if (!ref.current) {
      return;
    }

    const fitAddon = new FitAddon();

    terminal.loadAddon(fitAddon);
    terminal.open(ref.current);
    fitAddon.fit();

    ref.current.addEventListener("resize", () => fitAddon.fit());
  }, [ref.current]);

  const line: string[] = [];

  useEffect(() => {
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
  }, []);

  useEffect(() => {
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
  }, []);

  useEffect(() => {
    void (async () => {
      for await (const data of outputs[0]) {
        terminal.write(data === "\n" ? "\r\n" : data);
      }
    })();
  }, [outputs]);

  return <div class={styles.root} id={id} ref={ref} />;
};
