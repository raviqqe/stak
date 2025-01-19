import { useStore } from "@nanostores/solid";
import type { JSX } from "solid-js";
import * as store from "../../stores/interpreter-demo.js";
import { ErrorMessage } from "../ErrorMessage.js";
import { Field } from "../Field.jsx";
import { Label } from "../Label.jsx";
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
