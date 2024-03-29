import { createMemo, type Accessor, type JSX } from "solid-js";
import styles from "./DemoIo.module.css";
import { ErrorMessage } from "./ErrorMessage";
import { Label } from "./Label";
import { Link } from "./Link";
import { TextArea } from "./TextArea";

interface Props {
  input: Accessor<string>;
  output: Accessor<Uint8Array | null>;
  outputUrl: Accessor<string | null>;
  interpreterError: Accessor<string>;
  onInputChange: (input: string) => void;
  style?: JSX.CSSProperties;
}

export const DemoIo = ({
  input,
  interpreterError,
  output,
  outputUrl,
  onInputChange,
  style,
}: Props): JSX.Element => {
  const decoder = new TextDecoder();
  const textOutput = createMemo(() => {
    const value = output();
    return value ? decoder.decode(value) : "";
  });

  return (
    <div class={styles.container} style={style}>
      <Label for="input">stdin</Label>
      <TextArea
        id="input"
        onChange={onInputChange}
        style={{ flex: 1 }}
        value={input()}
      />
      <Label for="output">stdout</Label>
      <pre class={styles.output} id="output">
        {textOutput()}
      </pre>
      {outputUrl() && <Link href={outputUrl() ?? ""}>Download</Link>}
      <ErrorMessage>{interpreterError()}</ErrorMessage>
    </div>
  );
};
