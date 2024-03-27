import { DemoForm } from "./DemoForm";
import { DemoOutput } from "./DemoOutput";
import styles from "./Demo.module.css";
import type { JSX } from "preact";

export const Demo = (): JSX.Element => (
  <div class={styles.container}>
    <DemoForm />
    <DemoOutput />
  </div>
);
