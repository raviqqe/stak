import { useStore } from "@nanostores/preact";
import type { WritableAtom } from "nanostores";
import styles from "./DemoForm.module.css";
import type { JSX } from "preact/jsx-runtime";

interface Props {
  source: WritableAtom<string>;
}

export const DemoForm = ({ source }: Props): JSX.Element => {
  const $source = useStore(source);

  return (
    <form
      class={styles.container}
      onSubmit={() => {
        source.set("foo");
      }}
    >
      <textarea onChange={(event) => source.set(event.currentTarget.value)}>
        {$source}
      </textarea>
      <div class={styles.buttonGroup}>
        <button>Run</button>
      </div>
    </form>
  );
};
