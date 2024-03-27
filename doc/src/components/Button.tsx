import type { ComponentChildren, JSX } from "preact";
import styles from "./Button.module.css";

interface Props {
  children: ComponentChildren;
  onClick: () => void;
}

export const Button = ({ children, onClick }: Props): JSX.Element => (
  <button
    class={styles.container}
    onClick={(event) => {
      event.preventDefault();
      onClick();
    }}
  >
    {children}
  </button>
);
