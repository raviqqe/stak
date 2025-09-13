import type { FunctionComponent } from "preact";
import styles from "./ErrorMessage.module.css";

export const ErrorMessage: FunctionComponent = ({ children }) => (
  <>{children && <pre class={styles.root}>{children}</pre>}</>
);
