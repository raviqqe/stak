import { useStore } from "@nanostores/solid";
import type { JSX } from "solid-js/jsx-runtime";
import { $source, initializeWorker } from "../stores/demo-store";
import styles from "./DemoForm.module.css";

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
        {source()}
      </textarea>
      <div>
        <button type="submit">Run</button>
      </div>
    </form>
  );
};
