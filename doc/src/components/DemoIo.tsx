import { useStore } from "@nanostores/preact";
import type { JSX } from "preact";
import type { CSSProperties } from "preact/compat";
import { inputStore, outputStore, outputUrlStore } from "../stores/demo-store";
import styles from "./DemoIo.module.css";
import { Label } from "./Label";
import { Link } from "./Link";
import { TextArea } from "./TextArea";

interface Props {
  style?: CSSProperties;
}

export const DemoIo = ({ style }: Props): JSX.Element => {
  const input = useStore(inputStore);
  const output = useStore(outputStore);
  const outputUrl = useStore(outputUrlStore);

  return (
    <div class={styles.container} style={style}>
      <Label for="input">stdin</Label>
      <TextArea
        id="input"
        onChange={(input) => inputStore.set(input)}
        value={input}
      />
      <Label for="output">stdout</Label>
      <p class={styles.output} id="output">
        {output}
      </p>
      {outputUrl && <Link href={outputUrl}>Download</Link>}
    </div>
  );
};
