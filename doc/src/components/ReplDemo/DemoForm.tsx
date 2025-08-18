import { useStore } from "@nanostores/preact";
import type { JSX } from "preact";
import * as store from "../../stores/repl.js";
import { Field } from "../Field.js";
import { Label } from "../Label.js";
import { Terminal } from "../Terminal.js";
import styles from "./DemoForm.module.css";
import { effect } from "@preact/signals";

const source = [
  "(import (scheme base))",
  '(write-string "Hello, world!\\n")',
  "(define (fibonacci x)",
  "\t(if (< x 2)",
  "\t\tx",
  "\t\t(+",
  "\t\t\t(fibonacci (- x 1))",
  "\t\t\t(fibonacci (- x 2))))) ",
  "(fibonacci 10)",
];

export const DemoForm = (): JSX.Element => {
  const input = new TransformStream<string, string>();
  const output = useStore(store.output);

  effect(() =>
    store.input.set(input.readable.pipeThrough(new TextEncoderStream())),
  );

  return (
    <form class={styles.root}>
      <Field style={{ flex: 1 }}>
        <Label for="repl">Try it out!</Label>
        <Terminal
          id="repl"
          initialInput={source}
          input={input.writable}
          output={output.pipeThrough(new TextDecoderStream())}
        />
      </Field>
    </form>
  );
};
