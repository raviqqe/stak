import { useStore } from "@nanostores/preact";
import styles from "./DemoForm.module.css";
import type { JSX } from "preact/jsx-runtime";
import { $source } from "../stores/demo-store";

export const DemoForm = (): JSX.Element => {
  const source = useStore($source);

  return (
    <form class={styles.container}>
      <textarea onInput={(event) => $source.set(event.currentTarget.value)}>
        {source}
      </textarea>
      <div>
        <button type="submit">Run</button>
      </div>
    </form>
  );
};
