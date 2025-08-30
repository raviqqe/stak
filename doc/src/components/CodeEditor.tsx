import "monza-editor/style.css";
import { Editor } from "@monza-editor/preact";
import classNames from "classnames";
import { highlight } from "picolight";
import { scheme } from "picolight/languages/scheme";
import { githubDark } from "picolight/themes/github-dark";
import { githubLight } from "picolight/themes/github-light";
import type { FunctionComponent } from "preact";
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
    onHighlight={(text) =>
      highlight(
        text,
        scheme,
        document.querySelector("html")?.getAttribute("data-theme") === "dark"
          ? githubDark
          : githubLight,
      ).outerHTML
    }
    onInput={(event) => onInput(event.target.value)}
  />
);
