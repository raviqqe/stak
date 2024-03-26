import { useStore } from "@nanostores/preact";
import type { ReadableAtom } from "nanostores";
import type { JSX } from "preact/jsx-runtime";

interface Props {
  output: ReadableAtom<string>;
}

export const DemoOutput = ({ output }: Props): JSX.Element => {
  const $output = useStore(output);

  return (
    <pre>
      <code>{$output}</code>
    </pre>
  );
};
