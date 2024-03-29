import { Show, type Accessor, type JSX } from "solid-js";
import styles from "./ErrorMessage.module.css";

interface Props {
  children: Accessor<JSX.Element>;
}

export const ErrorMessage = ({ children }: Props): JSX.Element => (
  <Show when={children()}>
    <p class={styles.container}>{children()}</p>
  </Show>
);
