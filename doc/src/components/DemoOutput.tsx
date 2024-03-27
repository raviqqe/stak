import { useStore } from "@nanostores/solid";
import { $output } from "../stores/demo-store";
import styles from "./DemoOutput.module.css";
import type { JSX } from "solid-js/jsx-runtime";

export const DemoOutput = (): JSX.Element => {
  const output = useStore($output);

  return (
    <div class={styles.container}>
      <pre class={styles.output}>
        <code>{output()}</code>
      </pre>
    </div>
  );
};
