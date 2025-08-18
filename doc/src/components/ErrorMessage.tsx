import type { JSX } from "preact";
import styles from "./ErrorMessage.module.css";

export const ErrorMessage = (
  props: JSX.ElementChildrenAttribute,
): JSX.Element => (
  <>{props.children && <p class={styles.root}>{props.children}</p>}</>
);
