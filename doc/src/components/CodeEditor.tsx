import Quill from "quill";
import hljs from "highlight.js";
import { createUniqueId, onMount, type JSX } from "solid-js";

interface Props {
  onChange: (text: string) => void;
}

export const CodeEditor = ({ onChange }: Props): JSX.Element => {
  const id = createUniqueId();
  let quill: Quill | undefined;

  onMount(() => {
    quill = new Quill(id, {
      modules: {
        syntax: { hljs },
      },
    });
  });

  return <div id={id}></div>;
};
