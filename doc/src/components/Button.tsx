import classNames from "classnames/bind";
import type { ComponentChildren, JSX } from "preact";
import styles from "./Button.module.css";

const classes = classNames.bind(styles);

interface Props {
  icon?: ComponentChildren;
  children: ComponentChildren;
  disabled?: boolean;
  onClick: () => void;
}

export const Button = ({
  icon,
  children,
  disabled,
  onClick,
}: Props): JSX.Element => (
  <button
    class={classes(styles.container, { disabled })}
    onClick={(event) => {
      event.preventDefault();
      onClick();
    }}
  >
    {icon}
    {children}
  </button>
);
