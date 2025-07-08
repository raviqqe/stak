import { onMount, type JSX } from "solid-js";
import * as xterm from "@xterm/xterm";

interface Props {
  input: ReadableStream;
  output: ReadableStream;
}

export const Terminal = (props: Props): JSX.Element => {
  const terminal = new xterm.Terminal();
  let element: HTMLDivElement | null = null;

  onMount(() => {
    if (element) {
      terminal.open(element);
    }
  });

  return (
    <div
      ref={(value) => {
        element = value;
      }}
    />
  );
};
