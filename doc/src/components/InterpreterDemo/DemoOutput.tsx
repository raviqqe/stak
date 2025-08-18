import { useStore } from "@nanostores/preact";
import type { JSX } from "preact";
import * as store from "../../stores/interpreter.js";
import { ErrorMessage } from "../ErrorMessage.js";
import { Field } from "../Field.js";
import { Label } from "../Label.js";
import styles from "./DemoOutput.module.css";

export const DemoOutput = (): JSX.Element => {
  const output = useStore(store.output);
  const error = useStore(store.error);

  return (
    <div class={styles.root}>
      <Field>
        <Label for="output">Output</Label>
        <pre class={styles.output} id="output">
          {output()}
        </pre>
        {error() && <ErrorMessage>{error()?.message}</ErrorMessage>}
      </Field>
    </div>
  );
};
