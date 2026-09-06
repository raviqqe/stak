import "monza-editor/style.css";
import { Editor } from "@monza-editor/preact";
import classNames from "classnames";
import type { FunctionComponent } from "preact";
import { highlightScheme } from "../application/highlight.js";
import styles from "./CodeEditor.module.css";

interface Props {
  class?: string;
  id?: string;
  onInput: (text: string) => void;
  value?: string;
}

export const CodeEditor: FunctionComponent<Props> = ({ onInput, ...props }) => (
  <Editor
    {...props}
    class={classNames(styles.main, props.class)}
    onHighlight={highlightScheme}
    onInput={(event) => {
      if (event.target instanceof HTMLTextAreaElement) {
        onInput(event.target.value);
      }
    }}
  />
);
