import type { JSX } from "preact";
import styles from "./TextArea.module.css";
import type { CSSProperties } from "preact/compat";

interface Props {
  id?: string;
  onChange: (value: string) => void;
  style?: CSSProperties;
  value: string;
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
