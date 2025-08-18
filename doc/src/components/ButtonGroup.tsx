import type { JSX } from "preact";
import styles from "./ButtonGroup.module.css";

interface Props {
  children: JSX.Element;
}

export const ButtonGroup = (props: Props): JSX.Element => (
  <div class={styles.root}>{props.children}</div>
);
