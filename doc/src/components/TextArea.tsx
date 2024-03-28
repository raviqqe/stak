import type { JSX } from "preact";
import styles from "./TextArea.module.css";

interface Props {
  id?: string;
  defaultValue?: string;
  onChange: (value: string) => void;
}

export const TextArea = ({
  id,
  defaultValue,
  onChange,
}: Props): JSX.Element => (
  <textarea
    class={styles.container}
    id={id}
    defaultValue={defaultValue}
    onChange={(event) => onChange(event.currentTarget.value)}
  />
);
