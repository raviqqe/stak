import { type JSX } from "solid-js";
import styles from "./Message.module.css";

interface Props {
  children?: JSX.Element;
}

export const Message = ({ children }: Props): JSX.Element => (
  <div class={styles.container}>{children}</div>
);
