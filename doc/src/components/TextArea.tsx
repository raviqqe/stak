import classNames from "classnames";
import type { FunctionComponent, JSX } from "preact";
import styles from "./TextArea.module.css";

interface Props {
  class?: string;
  id?: string;
  onChange: (value: string) => void;
  style?: JSX.CSSProperties;
  value?: string;
}

export const TextArea: FunctionComponent<Props> = ({
  id,
  onChange,
  style,
  value,
  ...props
}) => (
  <textarea
    class={classNames(styles.root, props.class)}
    id={id}
    onInput={(event) => onChange(event.currentTarget.value)}
    style={style}
    value={value}
  >
    {value}
  </textarea>
);
