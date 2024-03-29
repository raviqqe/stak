import { createSignal, type JSX } from "solid-js";
import styles from "./Demo.module.css";
import { DemoForm } from "./DemoForm";
import { DemoIo } from "./DemoIo";
import { compile as compileProgram } from "../application/compile.js";
import { interpret as interpretProgram } from "../application/interpret.js";

const defaultSource = `
(import (scheme base) (scheme read) (scheme write))

(display "Hello, world!")
`.trim();

export const Demo = (): JSX.Element => {
  const [source, setSource] = createSignal(defaultSource);
  const [bytecodes, setBytecodes] = createSignal<Uint8Array | null>(
    new Uint8Array(),
  );
  const [input, setInput] = createSignal("");
  const [output, setOutput] = createSignal<Uint8Array | null>(new Uint8Array());
  const [compilerError, setCompilerError] = createSignal("");
  const [interpreterError, setInterpreterError] = createSignal("");

  return (
    <div class={styles.container}>
      <DemoForm
        bytecodes={bytecodes}
        compilerError={compilerError}
        onCompile={async () => {
          setBytecodes(null);
          setCompilerError("");

          let bytecodes = new Uint8Array();

          try {
            bytecodes = await compileProgram(source());
          } catch (error) {
            setCompilerError((error as Error).message);
          }

          setBytecodes(bytecodes);
        }}
        onInterpret={async () => {
          const value = bytecodes();

          if (!value) {
            return;
          }

          setOutput(null);
          setInterpreterError("");

          let output = new Uint8Array();

          try {
            output = await interpretProgram(
              value,
              new TextEncoder().encode(input()),
            );
          } catch (error) {
            setInterpreterError((error as Error).message);
          }

          setOutput(output);
        }}
        onSourceChange={setSource}
        output={output}
        source={source}
      />
      <DemoIo
        input={input}
        interpreterError={interpreterError}
        onInputChange={setInput}
        output={output}
        style={{
          // eslint-disable-next-line @typescript-eslint/naming-convention
          "max-width": "50%",
        }}
      />
    </div>
  );
};
