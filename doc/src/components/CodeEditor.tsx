import { Editor } from "monza-editor";
import { createEffect, type JSX, onMount } from "solid-js";

interface Props {
  autoBrackets?: boolean;
  class?: string;
  id?: string;
  onChange: (text: string) => void;
  value?: string;
}

export const CodeEditor = (props: Props): JSX.Element => {
  onMount(() => {
    instance = editor.create(element, {
      autoClosingBrackets: props.autoBrackets === false ? "never" : undefined,
      automaticLayout: true,
      language: "scheme",
      lineNumbers: "off",
      minimap: { enabled: false },
      theme: "vs-dark",
      value: props.value,
    });
    const model = instance.getModel();
    const onChange = props.onChange;

    model?.onDidChangeContent(() => onChange(model.getValue()));
  });

  createEffect(() => {
    if (props.value) {
      instance?.setValue(props.value);
      instance?.setPosition({
        column: Number.POSITIVE_INFINITY,
        lineNumber: Number.POSITIVE_INFINITY,
      });
    }
  });

  return <Editor class={props.class} />;
};
