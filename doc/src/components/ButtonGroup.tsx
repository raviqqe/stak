import { type ComponentChildren, type JSX } from "solid-js";
import styles from "./ButtonGroup.module.css";

interface Props {
  children: ComponentChildren;
}

export const ButtonGroup = ({ children }: Props): JSX.Element => (
  <div class={styles.container}>{children}</div>
);
