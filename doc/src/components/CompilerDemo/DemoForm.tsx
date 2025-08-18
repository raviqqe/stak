import { useStore } from "@nanostores/solid";
import { Boxes, CirclePlay } from "lucide-solid";
import type { JSX } from "preact";
import * as store from "../../stores/compiler.js";
import { Button } from "../Button.js";
import { ButtonGroup } from "../ButtonGroup.js";
import { CodeEditor } from "../CodeEditor.js";
import { ErrorMessage } from "../ErrorMessage.js";
import { Field } from "../Field.js";
import { Label } from "../Label.js";
import styles from "./DemoForm.module.css";

export const DemoForm = (): JSX.Element => {
  const source = useStore(store.source);
  const bytecodesReady = useStore(store.bytecodesReady);
  const compiling = useStore(store.compiling);
  const interpreting = useStore(store.interpretingStore);
  const error = useStore(store.compilerError);

  return (
    <form class={styles.root}>
      <Field style={{ flex: 1 }}>
        <Label for="source">Program</Label>
        <CodeEditor
          class={styles.program}
          id="source"
          onInput={(source) => store.source.set(source)}
          value={source()}
        />
        <ErrorMessage>{error()}</ErrorMessage>
      </Field>
      <ButtonGroup>
        <Button disabled={compiling()} icon={<Boxes />} onClick={store.compile}>
          {compiling() ? "Compiling..." : "Compile"}
        </Button>
        <Button
          disabled={!bytecodesReady() || interpreting()}
          icon={<CirclePlay />}
          onClick={store.interpret}
        >
          {interpreting() ? "Interpreting..." : "Interpret"}
        </Button>
      </ButtonGroup>
    </form>
  );
};
