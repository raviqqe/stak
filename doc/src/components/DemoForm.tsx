import { useStore } from "@nanostores/solid";
import { createSignal, onCleanup, onMount } from "solid-js";
import type { JSX } from "solid-js/jsx-runtime";
import {
  $source,
  initializeCompilerWorker,
  initializeInterpreterWorker,
  compile,
  interpret,
} from "../stores/demo-store";
import styles from "./DemoForm.module.css";

export const DemoForm = (): JSX.Element => {
  const source = useStore($source);
  const [workers, setWorkers] = createSignal<Worker[]>([]);

  onMount(() =>
    setWorkers([initializeCompilerWorker(), initializeInterpreterWorker()]),
  );

  onCleanup(() => {
    for (const worker of workers()) {
      worker.terminate();
    }
  });

  return (
    <form class={styles.container}>
      <textarea
        class={styles.source}
        onInput={(event) => $source.set(event.currentTarget.value)}
      >
        {source()}
      </textarea>
      <div>
        <button onChange={() => compile()}>Compile</button>
        <button onChange={() => interpret()}>Run</button>
      </div>
    </form>
  );
};
