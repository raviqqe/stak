import type { JSX } from "preact";
import styles from "./ButtonGroup.module.css";

export const ButtonGroup = (
  props: JSX.ElementChildrenAttribute,
): JSX.Element => <div class={styles.root}>{props.children}</div>;
