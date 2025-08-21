import type { FunctionComponent } from "preact";
import styles from "./Link.module.css";

export const Link: FunctionComponent<{ href: string }> = ({
  children,
  href,
}) => (
  <a class={styles.root} href={href} rel="noreferrer" target="_blank">
    {children}
  </a>
);
