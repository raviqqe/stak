import { useStore } from "@nanostores/preact";
import type { JSX } from "preact/jsx-runtime";
import { $output } from "../stores/demo-store";
import styles from "./DemoOutput.module.css";

export const DemoOutput = (): JSX.Element => {
  const output = useStore($output);

  return (
    <div className={styles.container}>
      <pre className={styles.output}>
        <code>{output}</code>
      </pre>
    </div>
  );
};
