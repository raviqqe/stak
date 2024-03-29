import { useStore } from "@nanostores/preact";
import type { JSX } from "preact";
import {
  inputStore,
  interpreterErrorStore,
  outputStore,
  outputUrlStore,
} from "../stores/demo-store";
import styles from "./DemoIo.module.css";
import { ErrorMessage } from "./ErrorMessage";
import { Label } from "./Label";
import { Link } from "./Link";
import { TextArea } from "./TextArea";

export const DemoIo = (): JSX.Element => {
  const input = useStore(inputStore);
  const output = useStore(outputStore);
  const outputUrl = useStore(outputUrlStore);
  const error = useStore(interpreterErrorStore);

  return (
    <div class={styles.container}>
      <Label for="input">stdin</Label>
      <TextArea
        id="input"
        onChange={(input) => inputStore.set(input)}
        value={input}
      />
      <Label for="output">stdout</Label>
      <p class={styles.output} id="output">
        <pre>{output}</pre>
      </p>
      {outputUrl && <Link href={outputUrl}>Download</Link>}
      <ErrorMessage>{error}</ErrorMessage>
    </div>
  );
};
