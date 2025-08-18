import type { JSX } from "preact";
import styles from "./Label.module.css";

interface Props extends JSX.ElementChildrenAttribute {
  for: string;
}

export const Label = (props: Props): JSX.Element => (
  <label class={styles.root} for={props.for}>
    {props.children}
  </label>
);
