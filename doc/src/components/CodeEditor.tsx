import { editor } from "monaco-editor";
import { type JSX, createMemo, createUniqueId, onMount } from "solid-js";

interface Props {
  class?: string;
  id?: string;
  onChange: (text: string) => void;
  value?: string;
}

export const CodeEditor = (props: Props): JSX.Element => {
  const id = createMemo(() => props.id ?? createUniqueId());

  onMount(() => {
    const element = document.getElementById(id());

    if (!element) {
      throw new Error("Editor element not found");
    }

    const model = editor
      .create(element, {
        automaticLayout: true,
        language: "scheme",
        lineNumbers: "off",
        minimap: { enabled: false },
        theme: "vs-dark",
        value: props.value,
      })
      .getModel();
    const onChange = props.onChange;

    model?.onDidChangeContent(() => onChange(model.getValue()));
  });

  return <div class={props.class} id={id()} />;
};
