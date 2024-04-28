import type { JSX } from "solid-js";
import styles from "./Link.module.css";

interface Props {
  href: string;
  children: JSX.Element;
}

export const Link = (props: Props): JSX.Element => (
  <a
    class={styles.container}
    href={props.href}
    rel="noreferrer"
    target="_blank"
  >
    {props.children}
  </a>
);
