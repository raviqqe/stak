import { useStore } from "@nanostores/preact";
import type { FunctionComponent } from "preact";
import * as store from "../../stores/interpreter.js";
import { ErrorMessage } from "../ErrorMessage.js";
import { Field } from "../Field.js";
import { Label } from "../Label.js";
import styles from "./DemoOutput.module.css";

export const DemoOutput: FunctionComponent = () => {
  const output = useStore(store.output);

  return (
    <div class={styles.root}>
      <Field>
        <Label for="output">Output</Label>
        <pre class={styles.output} id="output">
          {output.state === "ready" ? output.value : null}
        </pre>
        {output.state === "failed" && output.error instanceof Error && (
          <ErrorMessage>{output.error.message}</ErrorMessage>
        )}
      </Field>
    </div>
  );
};
