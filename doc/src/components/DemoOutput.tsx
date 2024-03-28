import { useStore } from "@nanostores/preact";
import type { JSX } from "preact";
import { outputStore, outputUrlStore } from "../stores/demo-store";
import styles from "./DemoOutput.module.css";
import { Label } from "./Label";
import { Link } from "./Link";

export const DemoOutput = (): JSX.Element => {
  const output = useStore(outputStore);
  const outputUrl = useStore(outputUrlStore);

  return (
    <div class={styles.container}>
      <Label for="output">stdout</Label>
      <pre class={styles.output} id="output">
        <code>{output}</code>
      </pre>
      {output && <Link href={outputUrl}>Download</Link>}
    </div>
  );
};
