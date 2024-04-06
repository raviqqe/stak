import { type JSX } from "solid-js";
import styles from "./Field.module.css";

interface Props {
  children: JSX.Element;
}

export const Field = (props: Props): JSX.Element => (
  <div class={styles.container}>{props.children}</div>
);
