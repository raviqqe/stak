import type { FunctionComponent, JSX } from "preact";
import { useId } from "preact/hooks";
import { Field } from "../Field.js";
import { Label } from "../Label.js";
import { Terminal } from "../Terminal.js";
import styles from "./DemoForm.module.css";
import { runRepl } from "../../application/repl.js";

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

export const DemoForm: FunctionComponent = () => {
  const input = new TransformStream<string, string>();
  const output = runRepl(input.readable.pipeThrough(new TextEncoderStream()));
  const id = useId();

  return (
    <form class={styles.root}>
      <Field style={{ flex: 1 }}>
        <Label for={id}>Try it out!</Label>
        <Terminal
          id={id}
          initialInput={source}
          input={input.writable}
          output={output.pipeThrough(new TextDecoderStream())}
        />
      </Field>
    </form>
  );
};
