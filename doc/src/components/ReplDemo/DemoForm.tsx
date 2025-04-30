import { useStore } from "@nanostores/solid";
import {
  type JSX,
  createEffect,
  createMemo,
  createSignal,
  onMount,
} from "solid-js";
import * as store from "../../stores/repl.js";
import { CodeEditor } from "../CodeEditor";
import styles from "./DemoForm.module.css";

export const DemoForm = (): JSX.Element => {
  const output = useStore(store.output);
  const [sendInput, setSendInput] = createSignal<(bytes: Uint8Array) => void>();
  const [value, setValue] = createSignal("");
  const putInput = createMemo(() => (input: string) => {
    setValue((value) => value + input);
    sendInput()?.(new TextEncoder().encode(input));
  });

  onMount(() => {
    store.input.set(
      new ReadableStream({
        start: (controller) => {
          setSendInput(() => (bytes) => controller.enqueue(bytes));
        },
      }),
    );
  });

  createEffect(() => {
    if (value() == "> ") {
      putInput()?.("(import (scheme base))\n");
    }
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
      class={styles.root}
      id="source"
      onChange={(newValue) => {
        const input = newValue.slice(value().length);

        if (!input.includes("\n")) {
          return;
        }

        putInput()?.(input);
      }}
      value={value()}
    />
  );
};
