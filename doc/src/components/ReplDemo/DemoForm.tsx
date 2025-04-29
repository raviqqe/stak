import { useStore } from "@nanostores/solid";
import { type JSX, createEffect, createSignal, onMount } from "solid-js";
import * as store from "../../stores/repl.js";
import { CodeEditor } from "../CodeEditor";
import styles from "./DemoForm.module.css";

export const DemoForm = (): JSX.Element => {
  const output = useStore(store.output);
  const [putInput, setPutInput] = createSignal<(bytes: Uint8Array) => void>();
  const [value, setValue] = createSignal("");

  onMount(() => {
    store.input.set(
      new ReadableStream({
        start: (controller) => {
          setPutInput(() => (bytes) => controller.enqueue(bytes));
        },
      }),
    );
  });

  createEffect(() => {
    void (async () => {
      const decoder = new TextDecoder();

      for await (const chunk of output()) {
        setValue((value) => value + decoder.decode(chunk));
      }
    })();
  });

  return (
    <CodeEditor
      class={styles.textArea}
      id="source"
      onChange={(source) => putInput()?.(new TextEncoder().encode(source))}
      value={value()}
    />
  );
};
