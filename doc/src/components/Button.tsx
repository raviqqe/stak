import type { JSX } from "solid-js";
import styles from "./Button.module.css";

interface Props {
  children: JSX.Element;
  onClick: () => void;
}

export const Button = ({ children, onClick }: Props): JSX.Element => (
  <button class={styles.container} onClick={onClick}>
    {children}
  </button>
);
