import { useStore } from "@nanostores/preact";
import { useEffect } from "preact/hooks";
import type { JSX } from "preact/jsx-runtime";
import { $source, initializeWorker } from "../stores/demo-store";
import styles from "./DemoForm.module.css";

export const DemoForm = (): JSX.Element => {
  const source = useStore($source);

  useEffect(() => {
    const worker = initializeWorker();
    return () => worker.terminate();
  }, []);

  return (
    <form className={styles.container}>
      <textarea
        className={styles.source}
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
