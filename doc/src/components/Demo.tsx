import type { JSX } from "preact";
import styles from "./Demo.module.css";
import { DemoForm } from "./DemoForm";
import { DemoIo } from "./DemoIo";

export const Demo = (): JSX.Element => (
  <div class={styles.container}>
    <DemoForm />
    <DemoIo />
  </div>
);
