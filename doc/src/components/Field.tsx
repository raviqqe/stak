import { type JSX } from "solid-js";
import styles from "./Field.module.css";

interface Props {
  children: JSX.Element;
  style?: JSX.CSSProperties;
}

export const Field = (props: Props): JSX.Element => (
  <div class={styles.container} style={props.style}>
    {props.children}
  </div>
);
