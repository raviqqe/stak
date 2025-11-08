import type { CSSProperties, FunctionComponent } from "preact";
import styles from "./Field.module.css";

export const Field: FunctionComponent<{ style?: CSSProperties }> = ({
  children,
  style,
}) => (
  <div class={styles.root} style={style}>
    {children}
  </div>
);
