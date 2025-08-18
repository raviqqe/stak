import { useStore } from "@nanostores/preact";
import type { JSX } from "preact";
import * as store from "../../stores/compiler.js";
import { ErrorMessage } from "../ErrorMessage.js";
import { Field } from "../Field.js";
import { Label } from "../Label.js";
import { Link } from "../Link.js";
import { TextArea } from "../TextArea.js";
import styles from "./DemoIo.module.css";

export const DemoIo = (): JSX.Element => {
  const input = useStore(store.input);
  const output = useStore(store.textOutput);
  const outputUrl = useStore(store.outputUrlStore);
  const error = useStore(store.interpreterError);

  return (
    <div class={styles.root}>
      <Field style={{ flex: 1 }}>
        <Label for="input">stdin</Label>
        <TextArea
          class={styles.input}
          id="input"
          onChange={(input) => store.input.set(input)}
          style={{ flex: 1 }}
          value={input}
        />
      </Field>
      <Field style={{ flex: 1 }}>
        <Label for="output">stdout</Label>
        <pre class={styles.output} id="output">
          {output}
        </pre>
        {outputUrl && <Link href={outputUrl ?? ""}>Download</Link>}
        <ErrorMessage>{error}</ErrorMessage>
      </Field>
    </div>
  );
};
