import { useStore } from "@nanostores/solid";
import { type JSX, onMount } from "solid-js";
import * as store from "../../stores/repl.js";
import { Field } from "../Field.js";
import { Label } from "../Label.js";
import styles from "./DemoForm.module.css";
import { Terminal } from "../Terminal.js";

const _source = [
  "(import (scheme base) (scheme write))",
  '(write-string "Hello, world!\\n")',
  "(define (fibonacci x) (if (< x 2) x (+ (fibonacci (- x 1)) (fibonacci (- x 2)))))",
  "(fibonacci 10)",
];

export const DemoForm = (): JSX.Element => {
  const input = new TransformStream<Uint8Array, Uint8Array>();
  const output = useStore(store.output);

  onMount(() => store.input.set(input.readable));

  return (
    <form class={styles.root}>
      <Field style={{ flex: 1 }}>
        <Label for="repl">Try it out!</Label>
        <Terminal input={input.writable} output={output()} />
      </Field>
    </form>
  );
};
