import type { JSX } from "solid-js";
import styles from "./TextArea.module.css";

interface Props {
  id?: string;
  onChange: (value: string) => void;
  style?: JSX.CSSProperties;
  value?: string;
}

export const TextArea = (props: Props): JSX.Element => (
  <textarea
    class={styles.container}
    id={props.id}
    onChange={(event) => props.onChange(event.currentTarget.value)}
    style={props.style}
    value={props.value}
  >
    {props.value}
  </textarea>
);
