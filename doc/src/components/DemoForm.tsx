import { useStore } from "@nanostores/preact";
import type { WritableAtom } from "nanostores";

interface Props {
  source: WritableAtom<string>;
}

export const DemoForm = ({ source }: Props) => {
  const $source = useStore(source);

  return (
    <form
      class="container"
      onSubmit={() => {
        source.set("foo");
      }}
    >
      <textarea onChange={(event) => source.set(event.currentTarget.value)}>
        {$source}
      </textarea>
      <button>Run</button>
    </form>
  );
};
