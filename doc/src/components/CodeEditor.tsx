import { Editor } from "@monza-editor/solid";
import classNames from "classnames";
import { createHighlighterCore, createJavaScriptRegexEngine } from "shiki";
import type { JSX } from "preact";
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

export const CodeEditor = (props: Props): JSX.Element => (
  <Editor
    {...props}
    class={classNames(styles.main, props.class)}
    onHighlight={(text) =>
      highlighter.codeToHtml(text, {
        lang: "scheme",
        structure: "inline",
        theme: window.matchMedia("(prefers-color-scheme: dark)").matches
          ? "github-dark"
          : "github-light",
      })
    }
    onInput={(event) => props.onInput(event.target.value)}
  />
);
