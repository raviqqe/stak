import * as store from "../stores/repl.js";
import { createEffect, createSignal, type JSX } from "solid-js";
import { useStore } from "@nanostores/solid";
import { TextArea } from "./TextArea.js";

interface Props {
  class?: string;
}

export const ReplDemo = (props: Props): JSX.Element => {
  const stdin = useStore(store.stdin);
  const stdout = useStore(store.stdout);
  const [text, setText] = createSignal("");

  createEffect(async () => {
    for await (const output of stdout() ?? []) {
      setText(text + output);
    }
  });

  return (
    <TextArea
      class={props.class}
      onChange={(newText) => {
        console.log(newText, stdin());
        setText(newText);
        stdin()?.getWriter().write(newText.slice(text.length));
      }}
      value=""
    />
  );
};
