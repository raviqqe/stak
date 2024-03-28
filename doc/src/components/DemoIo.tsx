import { useStore } from "@nanostores/preact";
import type { JSX } from "preact";
import { inputStore, outputStore, outputUrlStore } from "../stores/demo-store";
import styles from "./DemoIo.module.css";
import { Label } from "./Label";
import { Link } from "./Link";

export const DemoIo = (): JSX.Element => {
  const input = useStore(inputStore);
  const output = useStore(outputStore);
  const outputUrl = useStore(outputUrlStore);

  return (
    <div class={styles.container}>
      <Label for="input">stdin</Label>
      <textarea
        class={styles.output}
        id="input"
        onInput={(event) => inputStore.set(event.currentTarget.value)}
      >
        {input}
      </textarea>
      <Label for="output">stdout</Label>
      <pre class={styles.output} id="output">
        <code>{output}</code>
      </pre>
      {outputUrl && <Link href={outputUrl}>Download</Link>}
    </div>
  );
};
