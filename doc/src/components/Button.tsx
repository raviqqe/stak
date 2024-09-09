import type { JSX } from "solid-js";
import styles from "./Button.module.css";

interface Props {
  children: JSX.Element;
  disabled?: boolean;
  icon?: JSX.Element;
  onClick: () => void;
}

export const Button = (props: Props): JSX.Element => (
  <button
    class={styles.container}
    disabled={props.disabled}
    onClick={() => props.onClick()}
    type="button"
  >
    {props.icon}
    {props.children}
  </button>
);
