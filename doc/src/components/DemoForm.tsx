import { useStore } from "@nanostores/solid";
import { Boxes, CirclePlay } from "lucide-solid";
import { Show, type JSX } from "solid-js";
import {
  compilerErrorStore,
  sourceStore,
  compilingStore,
  interpretingStore,
  compile,
  interpret,
} from "../stores/demo";
import { Button } from "./Button";
import { ButtonGroup } from "./ButtonGroup";
import styles from "./DemoForm.module.css";
import { ErrorMessage } from "./ErrorMessage";
import { Label } from "./Label";
import { TextArea } from "./TextArea";

export const DemoForm = (): JSX.Element => {
  const source = useStore(sourceStore);
  const compiling = useStore(compilingStore);
  const interpreting = useStore(interpretingStore);
  const error = useStore(compilerErrorStore);

  return (
    <form class={styles.container}>
      <Label for="source">Program</Label>
      <TextArea
        id="source"
        onChange={(source) => sourceStore.set(source)}
        style={{ flex: 1 }}
        value={source()}
      />
      <ErrorMessage>{error()}</ErrorMessage>
      <ButtonGroup>
        <Button disabled={compiling()} icon={<Boxes />} onClick={compile}>
          {compiling() ? "Compiling..." : "Compile"}
        </Button>
        <Button
          disabled={interpreting()}
          icon={<CirclePlay />}
          onClick={interpret}
        >
          {interpreting() ? "Interpreting..." : "Interpret"}
        </Button>
      </ButtonGroup>
    </form>
  );
};
