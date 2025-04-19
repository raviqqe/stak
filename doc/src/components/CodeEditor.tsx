import Quill from "quill";
import hljs from "highlight.js";
import { createUniqueId, onMount } from "solid-js";

export const CodeEditor = () => {
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
