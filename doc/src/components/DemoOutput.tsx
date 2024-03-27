import { useStore } from "@nanostores/solid";
import type { JSX } from "solid-js/jsx-runtime";
import { $interpreterOutput } from "../stores/demo-store";
import styles from "./DemoOutput.module.css";

export const DemoOutput = (): JSX.Element => {
  const output = useStore($interpreterOutput);

  return (
    <div class={styles.container}>
      <pre class={styles.output}>
        <code>{output()}</code>
      </pre>
    </div>
  );
};
