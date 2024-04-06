import { useStore } from "@nanostores/solid";
import { type JSX } from "solid-js";
import * as store from "../stores/demo";
import styles from "./DemoIo.module.css";
import { ErrorMessage } from "./ErrorMessage";
import { Field } from "./Field";
import { Label } from "./Label";
import { Link } from "./Link";
import { TextArea } from "./TextArea";

export const DemoIo = (): JSX.Element => {
  const input = useStore(store.input);
  const output = useStore(store.textOutput);
  const outputUrl = useStore(store.outputUrlStore);
  const error = useStore(store.interpreterError);

  return (
    <div class={styles.container}>
      <Field style={{ flex: 1 }}>
        <Label for="input">stdin</Label>
        <TextArea
          id="input"
          onChange={(input) => store.input.set(input)}
          style={{ flex: 1 }}
          value={input()}
        />
      </Field>
      <Field style={{ flex: 1 }}>
        <Label for="output">stdout</Label>
        <pre class={styles.output} id="output">
          {output()}
        </pre>
        {outputUrl() && <Link href={outputUrl() ?? ""}>Download</Link>}
        <ErrorMessage>{error()}</ErrorMessage>
      </Field>
    </div>
  );
};
