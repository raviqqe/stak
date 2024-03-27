import type { JSX } from "preact";
import styles from "./Demo.module.css";
import { DemoForm } from "./DemoForm";
import { DemoOutput } from "./DemoOutput";

export const Demo = (): JSX.Element => (
  <div class={styles.container}>
    <DemoForm />
    <DemoOutput />
  </div>
);
