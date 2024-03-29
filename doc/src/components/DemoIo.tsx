import { useStore } from "@nanostores/solid";
import { Show, type JSX } from "solid-js";
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
  const input = useStore(store.input);
  const output = useStore(store.textOutput);
  const outputUrl = useStore(store.outputUrlStore);
  const error = useStore(store.interpreterError);

  return (
    <div class={styles.container} style={props.style}>
      <Label for="input">stdin</Label>
      <TextArea
        id="input"
        onChange={(input) => store.input.set(input)}
        style={{ flex: 1 }}
        value={input()}
      />
      <Label for="output">stdout</Label>
      <pre class={styles.output} id="output">
        {output()}
      </pre>
      <Show when={outputUrl()}>
        <Link href={outputUrl() ?? ""}>Download</Link>
      </Show>
      <ErrorMessage>{error()}</ErrorMessage>
    </div>
  );
};
