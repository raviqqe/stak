import { Editor } from "@monza-editor/solid";
import type { JSX } from "solid-js";

interface Props {
  class?: string;
  id?: string;
  onChange: (text: string) => void;
  value?: string;
}

export const CodeEditor = (props: Props): JSX.Element => {
  return (
    <Editor
      class={props.class}
      onHighlight={(text: string) => text}
      id={props.id}
      {...props}
      onChange={(event) => props.onChange(event.target?.value ?? "")}
    />
  );
};
