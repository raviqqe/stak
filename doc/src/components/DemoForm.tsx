import { useStore } from "@nanostores/preact";
import type { WritableAtom } from "nanostores";
import styles from "./DemoForm.module.css";

interface Props {
  source: WritableAtom<string>;
}

export const DemoForm = ({ source }: Props) => {
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
