import type { FunctionComponent, JSX } from "preact";
import styles from "./Button.module.css";

interface Props {
  disabled?: boolean;
  icon?: JSX.Element;
  onClick: () => void;
}

export const Button: FunctionComponent<Props> = (props) => (
  <button
    class={styles.root}
    disabled={props.disabled}
    onClick={() => props.onClick()}
    type="button"
  >
    {props.icon}
    {props.children}
  </button>
);
