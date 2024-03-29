import { type ComponentChildren, type JSX } from "preact";
import styles from "./Link.module.css";

interface Props {
  href: string;
  children: ComponentChildren;
}

export const Link = ({ href, children }: Props): JSX.Element => (
  <a class={styles.container} href={href} rel="noreferrer" target="_blank">
    {children}
  </a>
);
