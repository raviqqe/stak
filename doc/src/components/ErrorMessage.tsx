import { type ComponentChildren, type JSX } from "solid-js";
import styles from "./ErrorMessage.module.css";

interface Props {
  children: ComponentChildren;
}

export const ErrorMessage = ({ children }: Props): JSX.Element => (
  <p
    class={styles.container}
    style={{ display: children ? undefined : "none" }}
  >
    {children}
  </p>
);
