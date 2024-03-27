import { type ComponentChildren, type JSX } from "preact";
import styles from "./Message.module.css";

interface Props {
  children: ComponentChildren;
}

export const Message = ({ children }: Props): JSX.Element => (
  <div class={styles.container}>{children}</div>
);
