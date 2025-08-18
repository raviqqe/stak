import type { FunctionComponent } from "preact";
import styles from "./ButtonGroup.module.css";

export const ButtonGroup: FunctionComponent = (props) => (
  <div class={styles.root}>{props.children}</div>
);
