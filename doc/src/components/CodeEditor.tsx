import Quill from "quill";
import hljs from "highlight.js";
import { createUniqueId, onMount, type JSX } from "solid-js";

interface Props {
  id?: string;
  onChange: (text: string) => void;
  value?: string;
}

export const CodeEditor = ({ id, value, onChange }: Props): JSX.Element => {
  id = id ?? createUniqueId();
  let quill: Quill | undefined;

  onMount(() => {
    quill = new Quill("#" + id, {
      modules: {
        syntax: { hljs },
      },
    });

    quill.setText(value ?? "");
    quill.on("text-change", () => onChange(quill?.getText() ?? ""));
  });

  return <pre id={id} />;
};
