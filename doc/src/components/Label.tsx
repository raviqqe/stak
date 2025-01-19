import type { JSX } from "solid-js";
import styles from "./Label.module.css";

interface Props {
  children: JSX.Element;
  for: string;
}

export const Label = (props: Props): JSX.Element => (
  <label class={styles.root} for={props.for}>
    {props.children}
  </label>
);
