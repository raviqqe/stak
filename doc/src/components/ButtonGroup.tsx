import { type JSX } from "solid-js";
import styles from "./ButtonGroup.module.css";

interface Props {
  children: JSX.Element[];
}

export const ButtonGroup = ({ children }: Props): JSX.Element => (
  <div class={styles.container}>{children}</div>
);
