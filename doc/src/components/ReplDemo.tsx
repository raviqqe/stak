import { useStore } from "@nanostores/solid";
import { createEffect, createSignal, onMount, type JSX } from "solid-js";
import { CodeEditor } from "./CodeEditor";
import * as store from "../stores/repl.js";

export const ReplDemo = (): JSX.Element => {
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

  createEffect(async () => {
    for await (const chunk of output()) {
      setValue((value) => value + chunk);
    }
  });

  return (
    <CodeEditor
      id="source"
      onChange={(source) => putInput()?.(new TextEncoder().encode(source))}
      value={value()}
    />
  );
};
