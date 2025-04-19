import * as monaco from "monaco-editor";
import { createUniqueId, onMount, type JSX } from "solid-js";

interface Props {
  id?: string;
  onChange: (text: string) => void;
  value?: string;
}

export const CodeEditor = ({ id, value, onChange }: Props): JSX.Element => {
  id = id ?? createUniqueId();

  onMount(() => {
    const element = document.getElementById(id);

    if (!element) {
      throw new Error("editor element not found");
    }

    monaco.editor.create(element, {
      value: "function hello() {\n\talert('Hello world!');\n}",
      language: "javascript",
    });
  });

  return <pre id={id} />;
};
