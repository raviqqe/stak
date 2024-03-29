import { Show, type JSX } from "solid-js";
import styles from "./ErrorMessage.module.css";

interface Props {
  children: JSX.Element;
}

export const ErrorMessage = (props: Props): JSX.Element => (
  <Show when={props.children}>
    <p class={styles.container}>{props.children}</p>
  </Show>
);
