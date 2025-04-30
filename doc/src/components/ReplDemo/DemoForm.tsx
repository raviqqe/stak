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

const prompt = "> ";
const promptPattern = new RegExp(prompt, "g");
const source = [
  "(import (scheme base) (scheme write))",
  '(write-string "Hello, world!\\n")',
];

export const DemoForm = (): JSX.Element => {
  const output = useStore(store.output);
  const [sendInput, setSendInput] = createSignal<(bytes: Uint8Array) => void>();
  const [value, setValue] = createSignal("");
  const putInput = createMemo(() => {
    const send = sendInput();

    return (input: string) => {
      setValue((value) => value + input);
      send?.(new TextEncoder().encode(input));
    };
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
    const put = putInput();

    for (const [index, line] of source.entries()) {
      if (
        value().match(promptPattern)?.length === index + 1 &&
        value().endsWith(prompt)
      ) {
        put(`${line}\n`);
      }
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
      autoBrackets={false}
      class={styles.root}
      id="source"
      onChange={(newValue) => {
        const input = newValue.slice(value().length);

        if (
          !input.includes("\n") &&
          input.match("(")?.length === input.match(")")?.length
        ) {
          return;
        }

        putInput()(input);
      }}
      value={value()}
    />
  );
};
