import { useStore } from "@nanostores/preact";
import type { JSX } from "preact/jsx-runtime";
import styles from "./DemoOutput.module.css";
import { $output } from "../stores/demo-store";

export const DemoOutput = (): JSX.Element => {
  const output = useStore($output);

  return (
    <div class={styles.container}>
      <pre class={styles.container}>
        <code>{output}</code>
      </pre>
    </div>
  );
};
