import type { ComponentChildren, JSX } from "preact";
import styles from "./Button.module.css";
import classNames from "classnames/bind";

const classes = classNames.bind(styles);

interface Props {
  children: ComponentChildren;
  disabled?: boolean;
  onClick: () => void;
}

export const Button = ({ children, disabled, onClick }: Props): JSX.Element => (
  <button
    class={classes(styles.container, { disabled })}
    onClick={(event) => {
      event.preventDefault();
      onClick();
    }}
  >
    {children}
  </button>
);
