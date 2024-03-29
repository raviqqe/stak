import { type JSX } from "solid-js";
import styles from "./Link.module.css";

interface Props {
  href: string;
  children: JSX.Element;
}

export const Link = ({ href, children }: Props): JSX.Element => (
  <a class={styles.container} href={href} rel="noreferrer" target="_blank">
    {children}
  </a>
);
