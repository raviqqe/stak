import { useStore } from "@nanostores/solid";
import { type JSX } from "solid-js";
import * as store from "../stores/demo";
import styles from "./DemoIo.module.css";
import { ErrorMessage } from "./ErrorMessage";
import { Label } from "./Label";
import { Link } from "./Link";
import { TextArea } from "./TextArea";

interface Props {
  style?: JSX.CSSProperties;
}

export const DemoIo = (props: Props): JSX.Element => {
  const input = useStore(store.inputStore);
  const output = useStore(store.outputStore);
  const outputUrl = useStore(store.outputUrlStore);
  const error = useStore(store.interpreterErrorStore);

  return (
    <div class={styles.container} style={props.style}>
      <Label for="input">stdin</Label>
      <TextArea
        id="input"
        onChange={(input) => store.inputStore.set(input)}
        style={{ flex: 1 }}
        value={input()}
      />
      <Label for="output">stdout</Label>
      <pre class={styles.output} id="output">
        {output()}
      </pre>
      {outputUrl() && <Link href={outputUrl() ?? ""}>Download</Link>}
      <ErrorMessage>{error()}</ErrorMessage>
    </div>
  );
};
