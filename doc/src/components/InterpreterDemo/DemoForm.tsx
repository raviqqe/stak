import { useStore } from "@nanostores/preact";
import type { FunctionComponent } from "preact";
import * as store from "../../stores/interpreter.js";
import { CodeEditor } from "../CodeEditor.js";
import { Field } from "../Field.js";
import { Label } from "../Label.js";
import styles from "./DemoForm.module.css";

export const DemoForm: FunctionComponent = () => {
  const source = useStore(store.source);

  return (
    <form class={styles.root}>
      <Field>
        <Label for="source">Program</Label>
        <CodeEditor
          class={styles.textArea}
          id="source"
          onInput={(source) => store.source.set(source)}
          value={source}
        />
      </Field>
    </form>
  );
};
