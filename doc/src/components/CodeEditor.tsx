import { editor } from "monaco-editor";
import {
  createEffect,
  createMemo,
  createUniqueId,
  type JSX,
  onMount,
} from "solid-js";

interface Props {
  autoBrackets?: boolean;
  class?: string;
  id?: string;
  onChange: (text: string) => void;
  value?: string;
}

export const CodeEditor = (props: Props): JSX.Element => {
  let instance: ReturnType<typeof editor.create> | undefined;

  const id = createMemo(() => props.id ?? createUniqueId());

  onMount(() => {
    const element = document.getElementById(id());

    if (!element) {
      throw new Error("Editor element not found");
    }

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

  return <div class={props.class} id={id()} />;
};
