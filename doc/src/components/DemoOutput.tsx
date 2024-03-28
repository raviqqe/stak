import { useStore } from "@nanostores/preact";
import type { JSX } from "preact";
import { outputStore } from "../stores/demo-store";
import styles from "./DemoOutput.module.css";
import { Label } from "./Label";

export const DemoOutput = (): JSX.Element => {
  const output = useStore(outputStore);

  return (
    <div class={styles.container}>
      <Label for="output">stdout</Label>
      <pre class={styles.output} id="output">
        <code>{output}</code>
      </pre>
    </div>
  );
};
