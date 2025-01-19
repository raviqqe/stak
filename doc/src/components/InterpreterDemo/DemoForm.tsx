import { useStore } from "@nanostores/solid";
import type { JSX } from "solid-js";
import * as store from "../../stores/interpreter-demo.js";
import { Field } from "../Field.jsx";
import { Label } from "../Label.jsx";
import { TextArea } from "../TextArea.jsx";
import styles from "./DemoForm.module.css";

export const DemoForm = (): JSX.Element => {
  const source = useStore(store.source);

  return (
    <form class={styles.root}>
      <Field>
        <Label for="source">Program</Label>
        <TextArea
          class={styles.textArea}
          id="source"
          onChange={(source) => store.source.set(source)}
          value={source()}
        />
      </Field>
    </form>
  );
};
