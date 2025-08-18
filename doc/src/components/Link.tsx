import type { FunctionComponent } from "preact";
import styles from "./Link.module.css";

export const Link: FunctionComponent<{ href: string }> = (props) => (
  <a class={styles.root} href={props.href} rel="noreferrer" target="_blank">
    {props.children}
  </a>
);
