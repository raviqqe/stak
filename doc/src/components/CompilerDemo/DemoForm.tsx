import { useStore } from "@nanostores/solid";
import { Boxes, CirclePlay } from "lucide-solid";
import type { JSX } from "solid-js";
import * as store from "../../stores/compiler-demo.js";
import { Button } from "../Button.jsx";
import { ButtonGroup } from "../ButtonGroup.jsx";
import { ErrorMessage } from "../ErrorMessage.jsx";
import { Field } from "../Field.jsx";
import { Label } from "../Label.jsx";
import styles from "./DemoForm.module.css";
import { CodeEditor } from "../CodeEditor.js";

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
          onChange={(source) => store.source.set(source)}
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
