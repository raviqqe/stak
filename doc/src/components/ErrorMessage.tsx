import { type JSX } from "solid-js";
import styles from "./ErrorMessage.module.css";

interface Props {
  children: JSX.Element;
}

export const ErrorMessage = ({ children }: Props): JSX.Element => (
  <p
    class={styles.container}
    style={{ display: children ? undefined : "none" }}
  >
    {children}
  </p>
);
