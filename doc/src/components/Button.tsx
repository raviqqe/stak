import type { FunctionComponent, JSX } from "preact";
import styles from "./Button.module.css";

interface Props {
  disabled?: boolean;
  icon?: JSX.Element;
  onClick: () => void;
}

export const Button: FunctionComponent<Props> = ({
  children,
  disabled,
  icon,
  onClick,
}) => (
  <button
    class={styles.root}
    disabled={disabled}
    onClick={() => onClick()}
    type="button"
  >
    {icon}
    {children}
  </button>
);
