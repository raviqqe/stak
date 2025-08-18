import type { JSX } from "preact";
import styles from "./ErrorMessage.module.css";

interface Props {
  children: JSX.Element;
}

export const ErrorMessage = (props: Props): JSX.Element => (
  <>{props.children && <p class={styles.root}>{props.children}</p>}</>
);
