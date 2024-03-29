import classNames from "classnames/bind";
import type { JSX } from "solid-js";
import styles from "./Button.module.css";

const classes = classNames.bind(styles);

interface Props {
  icon?: JSX.Element;
  children: JSX.Element;
  disabled?: boolean;
  onClick: () => void;
}

export const Button = (props: Props): JSX.Element => (
  <button
    class={classes(styles.container, { disabled: props.disabled })}
    onClick={(event) => {
      event.preventDefault();
      props.onClick();
    }}
  >
    {props.icon}
    {props.children}
  </button>
);
