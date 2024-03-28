import { useStore } from "@nanostores/preact";
import { type JSX } from "preact";
import {
  sourceStore,
  compilingStore,
  interpretingStore,
  compile,
  interpret,
} from "../stores/demo-store";
import { Button } from "./Button";
import { ButtonGroup } from "./ButtonGroup";
import styles from "./DemoForm.module.css";
import { Label } from "./Label";
import { TextArea } from "./TextArea";

const defaultSource = `
(import (scheme write))

(display "Hello, world!")
`.trim();

export const DemoForm = (): JSX.Element => {
  const compiling = useStore(compilingStore);
  const interpreting = useStore(interpretingStore);

  return (
    <form class={styles.container}>
      <Label for="source">Source</Label>
      <TextArea
        defaultValue={defaultSource}
        id="source"
        onChange={(source) => sourceStore.set(source)}
      />
      <ButtonGroup>
        <Button disabled={compiling} onClick={compile}>
          {compiling ? "Compiling..." : "Compile"}
        </Button>
        <Button disabled={interpreting} onClick={interpret}>
          {interpreting ? "Interpreting..." : "Interpret"}
        </Button>
      </ButtonGroup>
    </form>
  );
};
