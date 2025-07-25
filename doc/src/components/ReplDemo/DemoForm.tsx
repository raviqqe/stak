import { useStore } from "@nanostores/solid";
import { type JSX, onMount } from "solid-js";
import * as store from "../../stores/repl.js";
import { Field } from "../Field.js";
import { Label } from "../Label.js";
import { Terminal } from "../Terminal.js";
import styles from "./DemoForm.module.css";

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

  onMount(() =>
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
          output={output().pipeThrough(new TextDecoderStream())}
        />
      </Field>
    </form>
  );
};
