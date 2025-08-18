import type { FunctionComponent } from "preact";
import styles from "./Label.module.css";

export const Label: FunctionComponent<{ for: string }> = (props) => (
  <label class={styles.root} for={props.for}>
    {props.children}
  </label>
);
