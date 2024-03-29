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
import type { CSSProperties } from "preact/compat";

interface Props {
  style: CSSProperties;
}

export const DemoIo = ({ style }: Props): JSX.Element => {
  const input = useStore(inputStore);
  const output = useStore(outputStore);
  const outputUrl = useStore(outputUrlStore);
  const error = useStore(interpreterErrorStore);

  return (
    <div class={styles.container} style={style}>
      <Label for="input">stdin</Label>
      <TextArea
        id="input"
        onChange={(input) => inputStore.set(input)}
        value={input}
      />
      <Label for="output">stdout</Label>
      <pre class={styles.output} id="output">
        {output}
      </pre>
      {outputUrl && <Link href={outputUrl}>Download</Link>}
      <ErrorMessage>{error}</ErrorMessage>
    </div>
  );
};
