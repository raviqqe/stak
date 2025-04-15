import { useStore } from "@nanostores/solid";
import type { JSX } from "solid-js";
import * as store from "../../stores/compiler-demo.js";
import { ErrorMessage } from "../ErrorMessage.jsx";
import { Field } from "../Field.jsx";
import { Label } from "../Label.jsx";
import { Link } from "../Link.jsx";
import { TextArea } from "../TextArea.jsx";
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
