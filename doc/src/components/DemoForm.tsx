import { useStore } from "@nanostores/preact";
import styles from "./DemoForm.module.css";
import type { JSX } from "preact/jsx-runtime";
import { $source, initializeWorker } from "../stores/demo-store";
import { useEffect } from "preact/hooks";

export const DemoForm = (): JSX.Element => {
  const source = useStore($source);

  useEffect(() => {
    const worker = initializeWorker();
    return () => worker.terminate();
  }, []);

  return (
    <form class={styles.container}>
      <textarea
        class={styles.source}
        onInput={(event) => $source.set(event.currentTarget.value)}
      >
        {source}
      </textarea>
      <div>
        <button type="submit">Run</button>
      </div>
    </form>
  );
};
