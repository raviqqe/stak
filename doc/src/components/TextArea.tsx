import type { JSX } from "solid-js";
import styles from "./TextArea.module.css";

interface Props {
  id?: string;
  onChange: (value: string) => void;
  style?: JSX.CSSProperties;
  value?: string;
}

export const TextArea = ({
  id,
  onChange,
  style,
  value,
}: Props): JSX.Element => (
  <textarea
    class={styles.container}
    id={id}
    onChange={(event) => onChange(event.currentTarget.value)}
    style={style}
    value={value}
  >
    {value}
  </textarea>
);
