import type { FunctionComponent } from "preact";
import styles from "./ErrorMessage.module.css";

export const ErrorMessage: FunctionComponent = ({ children }) => (
  <>{children && <p class={styles.root}>{children}</p>}</>
);
