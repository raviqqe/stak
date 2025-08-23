import "monza-editor/style.css";
import { Editor } from "@monza-editor/preact";
import classNames from "classnames";
import type { FunctionComponent } from "preact";
import { createHighlighterCore, createJavaScriptRegexEngine } from "shiki";
import styles from "./CodeEditor.module.css";

const highlighter = await createHighlighterCore({
  engine: createJavaScriptRegexEngine(),
  langs: [import("@shikijs/langs/scheme")],
  themes: [
    import("@shikijs/themes/github-dark"),
    import("@shikijs/themes/github-light"),
  ],
});

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
      highlighter.codeToHtml(text, {
        lang: "scheme",
        structure: "inline",
        theme:
          document.querySelector("html")?.getAttribute("data-theme") === "dark"
            ? "github-dark"
            : "github-light",
      })
    }
    onInput={(event) => onInput(event.target.value)}
  />
);
