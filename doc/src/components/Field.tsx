import type { JSX } from "preact";
import styles from "./Field.module.css";

interface Props extends JSX.ElementChildrenAttribute {
  style?: JSX.CSSProperties;
}

export const Field = (props: Props): JSX.Element => (
  <div class={styles.root} style={props.style}>
    {props.children}
  </div>
);
