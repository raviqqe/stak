import type { FunctionComponent, JSX } from "preact";
import styles from "./Field.module.css";

export const Field: FunctionComponent<{ style?: JSX.CSSProperties }> = ({
  children,
  style,
}) => (
  <div class={styles.root} style={style}>
    {children}
  </div>
);
