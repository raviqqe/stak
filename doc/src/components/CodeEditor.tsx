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
    const element = document.getElementById(id);

    if (!element) {
      throw new Error("editor element not found");
    }

    const editor = monaco.editor.create(element, {
      value: "function hello() {\n\talert('Hello world!');\n}",
      language: "javascript",
    });
  });

  return <pre id={id} />;
};
