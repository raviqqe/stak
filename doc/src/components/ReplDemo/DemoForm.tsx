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
    void (async (output) => {
      const decoder = new TextDecoder();

      for await (const chunk of output) {
        setValue((value) => value + decoder.decode(chunk));
      }
    })(output());
  });

  return (
    <CodeEditor
      class={styles.textArea}
      id="source"
      onChange={(newValue) => {
        const input = newValue.slice(value.length);

        if (!input.includes("\n")) {
          return;
        }

        putInput()?.(new TextEncoder().encode(input));
      }}
      value={value()}
    />
  );
};
