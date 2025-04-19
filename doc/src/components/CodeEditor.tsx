import * as monaco from "monaco-editor";
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
    self.MonacoEnvironment = {
      getWorker: (workerId: string, label: string) => {
        const getWorkerModule = (_moduleUrl: string, label: string) => {
          const url = self.MonacoEnvironment?.getWorkerUrl?.(workerId, label);

          if (!url) {
            throw new Error("No worker URL");
          }

          return new Worker(url, {
            name: label,
            type: "module",
          });
        };

        switch (label) {
          case "scheme":
            return getWorkerModule(
              "/monaco-editor/esm/vs/language/scheme/scheme.worker?worker",
              label,
            );
          default:
            return getWorkerModule(
              "/monaco-editor/esm/vs/editor/editor.worker?worker",
              label,
            );
        }
      },
    };
    const element = document.getElementById(id());

    if (!element) {
      throw new Error("editor element not found");
    }

    monaco.editor.create(element, {
      automaticLayout: true,
      language: "scheme",
      lineNumbers: "off",
      minimap: { enabled: false },
      theme: "vs-dark",
      value: props.value,
    });
  });

  return <div class={props.class} id={id()} />;
};
