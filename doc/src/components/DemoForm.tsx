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
import { Message } from "./Message";
import { Label } from "./Label";

export const DemoForm = (): JSX.Element => {
  const source = useStore(sourceStore);
  const compiling = useStore(compilingStore);
  const interpreting = useStore(interpretingStore);

  return (
    <form class={styles.container}>
      <Label for="source">Source</Label>
      <textarea
        class={styles.source}
        id="source"
        onInput={(event) => sourceStore.set(event.currentTarget.value)}
      >
        {source}
      </textarea>
      <ButtonGroup>
        <Button onClick={compile}>Compile</Button>
        <Button onClick={interpret}>Interpret</Button>
      </ButtonGroup>
      <Message>
        {compiling ? "Compiling..." : interpreting ? "Interpreting..." : ""}
      </Message>
    </form>
  );
};
