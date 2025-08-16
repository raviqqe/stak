import { Editor } from "@monza-editor/solid";
import classNames from "classnames";
import type { JSX } from "solid-js";
import styles from "./CodeEditor.module.css";

interface Props {
  class?: string;
  id?: string;
  onChange: (text: string) => void;
  value?: string;
}

export const CodeEditor = (props: Props): JSX.Element => (
  <Editor
    {...props}
    class={classNames(styles.main, props.class)}
    onChange={(event) => props.onChange(event.target.value)}
    onHighlight={(text: string) => text}
  />
);
