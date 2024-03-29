import type { JSX } from "preact";
import styles from "./TextArea.module.css";

interface Props {
  id?: string;
  onChange: (value: string) => void;
  value: string;
}

export const TextArea = ({ id, onChange, value }: Props): JSX.Element => (
  <textarea
    class={styles.container}
    id={id}
    onChange={(event) => onChange(event.currentTarget.value)}
    value={value}
  >
    {value}
  </textarea>
);
