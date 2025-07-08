import { useStore } from "@nanostores/solid";
import { type JSX, onMount } from "solid-js";
import * as store from "../../stores/repl.js";
import { Field } from "../Field.js";
import { Label } from "../Label.js";
import styles from "./DemoForm.module.css";
import { Terminal } from "../Terminal.js";
import type { TransformStreamDefaultController } from "node:stream/web";

const _source = [
  "(import (scheme base) (scheme write))",
  '(write-string "Hello, world!\\n")',
  "(define (fibonacci x) (if (< x 2) x (+ (fibonacci (- x 1)) (fibonacci (- x 2)))))",
  "(fibonacci 10)",
];

class LineBufferedTransformer implements Transformer {
  private line = "";

  public transform(data: string, controller: TransformStreamDefaultController) {
    if (data === "\n") {
      controller.enqueue(this.line);
      this.line = "";
    } else {
      this.line += data;
    }
  }
}

export const DemoForm = (): JSX.Element => {
  const input = new TransformStream<string, Uint8Array>(
    new LineBufferedTransformer(),
  );
  const output = useStore(store.output);

  onMount(() => store.input.set(input.readable));

  return (
    <form class={styles.root}>
      <Field style={{ flex: 1 }}>
        <Label for="repl">Try it out!</Label>
        <Terminal
          input={input.writable}
          output={output().pipeThrough(new TextDecoderStream())}
        />
      </Field>
    </form>
  );
};
