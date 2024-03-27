import { useStore } from "@nanostores/preact";
import { type JSX } from "preact";
import {
  $source,
  $compiling,
  $interpreting,
  initializeCompilerWorker,
  initializeInterpreterWorker,
  compile,
  interpret,
} from "../stores/demo-store";
import { Button } from "./Button";
import { ButtonGroup } from "./ButtonGroup";
import styles from "./DemoForm.module.css";
import { Message } from "./Message";
import { useEffect } from "preact/hooks";

export const DemoForm = (): JSX.Element => {
  const source = useStore($source);
  const compiling = useStore($compiling);
  const interpreting = useStore($interpreting);

  useEffect(() => {
    const workers = [initializeCompilerWorker(), initializeInterpreterWorker()];
  });

  return (
    <form class={styles.container}>
      <textarea
        class={styles.source}
        onInput={(event) => $source.set(event.currentTarget.value)}
      >
        {source}
      </textarea>
      <ButtonGroup>
        <Button onClick={compile}>Compile</Button>
        <Button onClick={interpret}>Run</Button>
      </ButtonGroup>
      <Message>
        {compiling ? "Compiling..." : interpreting ? "Interpreting..." : ""}
      </Message>
    </form>
  );
};
