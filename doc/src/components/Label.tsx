import { type JSX } from "solid-js";
import styles from "./Label.module.css";

interface Props {
  children: JSX.Element;
  for: string;
}

export const Label = ({ children, for: forAttribute }: Props): JSX.Element => (
  <label class={styles.container} for={forAttribute}>
    {children}
  </label>
);
