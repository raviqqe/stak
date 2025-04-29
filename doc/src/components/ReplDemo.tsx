import { useStore } from "@nanostores/solid";
import { createEffect, createSignal, onMount, type JSX } from "solid-js";
import styles from "./DemoForm.module.css";
import { Field } from "./Field";
import { CodeEditor } from "./CodeEditor";
import { Label } from "./Label";
import * as store from "../stores/repl.js"

export const ReplDemo = (): JSX.Element => {
  const output = useStore(store.output);
  const [putInput, setPutInput] = createSignal<(bytes: Uint8Array) => void>();
  const [value, setValue] = createSignal("");

  onMount(() => {
    store.input.set(
      new ReadableStream({
        start: (controller) => {
          setPutInput(() => (bytes) => controller.enqueue(bytes))
        }
      }))
  })

  createEffect(async () => {
    for await (const chunk of output()) {
      setValue((value) => value + chunk)
    }
  })



  return (
    <form class={styles.root}>
      <Field>
        <Label for="source">Program</Label>
        <CodeEditor
          class={styles.textArea}
          id="source"
          onChange={(source) => putInput()?.(new TextEncoder().encode(source))}
          value={value()}
        />
      </Field>
    </form>
  );
};
