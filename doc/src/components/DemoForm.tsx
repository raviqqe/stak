import { useStore } from "@nanostores/solid";
import { Boxes, CirclePlay } from "lucide-solid";
import { type JSX } from "solid-js";
import * as store from "../stores/demo";
import { Button } from "./Button";
import { ButtonGroup } from "./ButtonGroup";
import styles from "./DemoForm.module.css";
import { ErrorMessage } from "./ErrorMessage";
import { Label } from "./Label";
import { TextArea } from "./TextArea";

export const DemoForm = (): JSX.Element => {
  const source = useStore(store.source);
  const compiling = useStore(store.compiling);
  const interpreting = useStore(store.interpretingStore);
  const error = useStore(store.compilerErrorStore);

  return (
    <form class={styles.container}>
      <Label for="source">Program</Label>
      <TextArea
        id="source"
        onChange={(source) => store.source.set(source)}
        style={{ flex: 1 }}
        value={source()}
      />
      <ErrorMessage>{error()}</ErrorMessage>
      <ButtonGroup>
        <Button disabled={compiling()} icon={<Boxes />} onClick={store.compile}>
          {compiling() ? "Compiling..." : "Compile"}
        </Button>
        <Button
          disabled={interpreting()}
          icon={<CirclePlay />}
          onClick={store.interpret}
        >
          {interpreting() ? "Interpreting..." : "Interpret"}
        </Button>
      </ButtonGroup>
    </form>
  );
};
