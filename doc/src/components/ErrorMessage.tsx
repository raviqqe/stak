import type { FunctionComponent } from "preact";
import styles from "./ErrorMessage.module.css";

export const ErrorMessage: FunctionComponent = (props) => (
  <>{props.children && <p class={styles.root}>{props.children}</p>}</>
);
