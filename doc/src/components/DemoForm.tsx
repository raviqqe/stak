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
import { useEffect } from "preact/hooks";
import { Boxes, CirclePlay } from "lucide-preact";

const defaultSource = `
(import (scheme write))

(display "Hello, world!")
`.trim();

export const DemoForm = (): JSX.Element => {
  const source = useStore(sourceStore);
  const compiling = useStore(compilingStore);
  const interpreting = useStore(interpretingStore);

  useEffect(() => {
    sourceStore.set(defaultSource);
  }, []);

  return (
    <form class={styles.container}>
      <Label for="source">Program</Label>
      <TextArea
        id="source"
        onChange={(source) => sourceStore.set(source)}
        value={source}
      />
      <ButtonGroup>
        <Button disabled={compiling} icon={<Boxes />} onClick={compile}>
          {compiling ? "Compiling..." : "Compile"}
        </Button>
        <Button
          disabled={interpreting}
          icon={<CirclePlay />}
          onClick={interpret}
        >
          {interpreting ? "Interpreting..." : "Interpret"}
        </Button>
      </ButtonGroup>
    </form>
  );
};
