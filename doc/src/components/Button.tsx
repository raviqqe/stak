import classNames from "classnames/bind";
import type { Accessor, JSX } from "solid-js";
import styles from "./Button.module.css";

const classes = classNames.bind(styles);

interface Props {
  icon?: JSX.Element;
  children: JSX.Element;
  disabled?: Accessor<boolean>;
  onClick: () => void;
}

export const Button = ({
  icon,
  children,
  disabled,
  onClick,
}: Props): JSX.Element => (
  <button
    class={classes(styles.container, { disabled: disabled?.() })}
    onClick={(event) => {
      event.preventDefault();
      onClick();
    }}
  >
    {icon}
    {children}
  </button>
);
