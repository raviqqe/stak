import type { FunctionComponent } from "preact";
import styles from "./Label.module.css";

export const Label: FunctionComponent<{ for: string }> = ({
  children,
  ...props
}) => (
  <label class={styles.root} for={props.for}>
    {children}
  </label>
);
