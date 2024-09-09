import type { JSX } from "solid-js";
import styles from "./Link.module.css";

interface Props {
  children: JSX.Element;
  href: string;
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
