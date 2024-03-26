import { useStore } from "@nanostores/preact";
import type { ReadableAtom } from "nanostores";
import type { JSX } from "preact/jsx-runtime";
import styles from "./DemoOutput.module.css";

interface Props {
  output: ReadableAtom<string>;
}

export const DemoOutput = ({ output }: Props): JSX.Element => {
  const $output = useStore(output);

  return (
    <pre class={styles.container}>
      <code>{$output}</code>
    </pre>
  );
};
