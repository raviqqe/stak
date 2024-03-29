import { type ComponentChildren, type JSX } from "solid-js";
import styles from "./Label.module.css";

interface Props {
  children: ComponentChildren;
  for: string;
}

export const Label = ({ children, for: forAttribute }: Props): JSX.Element => (
  <label class={styles.container} for={forAttribute}>
    {children}
  </label>
);
