import type { FunctionComponent } from "preact";
import styles from "./ButtonGroup.module.css";

export const ButtonGroup: FunctionComponent = ({ children }) => (
  <div class={styles.root}>{children}</div>
);
