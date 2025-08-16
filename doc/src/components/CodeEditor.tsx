import { Editor } from "@monza-editor/solid";
import type { JSX } from "solid-js";

interface Props {
  class?: string;
  id?: string;
  onChange: (text: string) => void;
  value?: string;
}

export const CodeEditor = (props: Props): JSX.Element => (
  <Editor
    {...props}
    onChange={(event) => props.onChange(event.target.value)}
    onHighlight={(text: string) => text}
  />
);
