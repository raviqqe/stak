import type { JSX } from "solid-js";
import styles from "./TextArea.module.css";
import classNames from "classnames";

interface Props {
  class?: string;
  id?: string;
  onChange: (value: string) => void;
  style?: JSX.CSSProperties;
  value?: string;
}

export const TextArea = (props: Props): JSX.Element => (
  <textarea
    class={classNames(styles.container, props.class)}
    id={props.id}
    onChange={(event) => props.onChange(event.currentTarget.value)}
    style={props.style}
    value={props.value}
  >
    {props.value}
  </textarea>
);
