import { Boxes, CirclePlay } from "lucide-solid";
import { createMemo, type Accessor, type JSX } from "solid-js";
import { Button } from "./Button";
import { ButtonGroup } from "./ButtonGroup";
import styles from "./DemoForm.module.css";
import { ErrorMessage } from "./ErrorMessage";
import { Label } from "./Label";
import { TextArea } from "./TextArea";

interface Props {
  bytecodes: Accessor<Uint8Array | null>;
  compilerError: Accessor<string>;
  onCompile: () => void;
  onInterpret: () => void;
  onSourceChange: (source: string) => void;
  output: Accessor<Uint8Array | null>;
  source: Accessor<string>;
}

export const DemoForm = ({
  bytecodes,
  compilerError,
  onCompile,
  onInterpret,
  onSourceChange,
  output,
  source,
}: Props): JSX.Element => {
  const compiling = createMemo(() => bytecodes() === null);
  const interpreting = createMemo(() => output() === null);

  return (
    <form class={styles.container}>
      <Label for="source">Program</Label>
      <TextArea
        id="source"
        onChange={onSourceChange}
        style={{ flex: 1 }}
        value={source()}
      />
      <ErrorMessage>{compilerError()}</ErrorMessage>
      <ButtonGroup>
        <Button disabled={compiling()} icon={<Boxes />} onClick={onCompile}>
          {compiling() ? "Compiling..." : "Compile"}
        </Button>
        <Button
          disabled={interpreting()}
          icon={<CirclePlay />}
          onClick={onInterpret}
        >
          {interpreting() ? "Interpreting..." : "Interpret"}
        </Button>
      </ButtonGroup>
    </form>
  );
};
