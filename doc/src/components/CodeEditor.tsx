import { Editor } from "@monza-editor/solid";
import classNames from "classnames";
import type { JSX } from "solid-js";
import styles from "./CodeEditor.module.css";
import { createHighlighterCore, createJavaScriptRegexEngine } from "shiki";

const highlighter = await createHighlighterCore({
  langs: [import("@shikijs/langs/scheme")],
  themes: [import("@shikijs/themes/nord")],
  engine: createJavaScriptRegexEngine(),
});

interface Props {
  class?: string;
  id?: string;
  onChange: (text: string) => void;
  value?: string;
}

export const CodeEditor = (props: Props): JSX.Element => {
  return (
    <Editor
      {...props}
      class={classNames(styles.main, props.class)}
      onChange={(event) => props.onChange(event.target.value)}
      onHighlight={(text) =>
        highlighter.codeToHtml(text, {
          lang: "scheme",
          structure: "inline",
          theme: "nord",
        })
      }
    />
  );
};
