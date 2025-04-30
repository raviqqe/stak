import { editor } from "monaco-editor";
import {
  type JSX,
  createEffect,
  createMemo,
  createUniqueId,
  onMount,
} from "solid-js";

interface Props {
  class?: string;
  id?: string;
  onChange: (text: string) => void;
  value?: string;
}

export const CodeEditor = (props: Props): JSX.Element => {
  let instance: ReturnType<typeof editor.create> | undefined;
  let model:
    | ReturnType<ReturnType<typeof editor.create>["getModel"]>
    | undefined;

  const id = createMemo(() => props.id ?? createUniqueId());

  onMount(() => {
    const element = document.getElementById(id());

    if (!element) {
      throw new Error("Editor element not found");
    }

    instance = editor.create(element, {
      automaticLayout: true,
      language: "scheme",
      lineNumbers: "off",
      minimap: { enabled: false },
      theme: "vs-dark",
      value: props.value,
    });
    model = instance.getModel();
    const onChange = props.onChange;

    model?.onDidChangeContent(() => {
      if (model) {
        onChange(model.getValue());
      }
    });
  });

  createEffect(() => {
    if (props.value) {
      model?.setValue(props.value);
      instance?.setPosition({ lineNumber: Infinity, column: Infinity });
    }
  });

  return <div class={props.class} id={id()} />;
};
