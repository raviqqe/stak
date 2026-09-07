import "monza-editor/style.css";
import { useSignalEffect } from "@preact/signals";
import { useSignalRef } from "@preact/signals/utils";
import classNames from "classnames";
import { delay } from "es-toolkit";
import { styles as editorStyles, initialize } from "monza-editor";
import type { FunctionComponent } from "preact";
import { highlightScheme } from "../application/highlight.js";
import styles from "./Terminal.module.css";

const inputDelay = 20;

interface Props {
  id?: string;
  initialInput?: string[];
  input: WritableStream<string>;
  output: ReadableStream<string>;
}

export const Terminal: FunctionComponent<Props> = ({
  id,
  initialInput,
  input,
  output,
}) => {
  const codeRef = useSignalRef<HTMLElement | null>(null);
  const preRef = useSignalRef<HTMLPreElement | null>(null);
  const textareaRef = useSignalRef<HTMLTextAreaElement | null>(null);

  useSignalEffect(() => {
    const code = codeRef.value;
    const pre = preRef.value;
    const textarea = textareaRef.value;

    if (!code || !pre || !textarea) {
      return;
    }

    initialize({ code, highlight: highlightScheme, pre, textarea });

    let frozen = 0;

    const update = (text: string) => {
      textarea.value = textarea.value.slice(0, frozen) + text;
      textarea.dispatchEvent(new InputEvent("input"));
      textarea.scrollTop = textarea.scrollHeight;
      frozen += text.length;
    };

    const writer = input.getWriter();

    const submit = async (line: string) => {
      const text = `${line}\n`;

      update(text);
      await writer.write(text);
    };

    textarea.addEventListener("beforeinput", (event) => {
      const { selectionEnd, selectionStart } = textarea;

      if (
        selectionStart < frozen ||
        (selectionStart === frozen &&
          selectionStart === selectionEnd &&
          event.inputType.endsWith("Backward"))
      ) {
        event.preventDefault();
      }
    });

    textarea.addEventListener("keydown", (event) => {
      if (event.key === "Enter") {
        event.preventDefault();

        void submit(textarea.value.slice(frozen));
      }
    });

    const outputs = output.tee();

    void (async () => {
      for await (const text of outputs[0]) {
        update(text);
      }
    })();

    void (async () => {
      await outputs[1].values().next();

      for (const line of initialInput ?? []) {
        await delay(inputDelay);
        await submit(line);
      }
    })();
  });

  return (
    <div class={classNames(styles.root, editorStyles.main)}>
      <textarea class={editorStyles.textarea} id={id} ref={textareaRef} />
      <pre class={editorStyles.pre} ref={preRef}>
        <code class={editorStyles.code} ref={codeRef} />
      </pre>
    </div>
  );
};
