import type { JSX } from "solid-js";
import styles from "./Demo.module.css";
import { DemoForm } from "./DemoForm";
import { DemoIo } from "./DemoIo";

export const Demo = (): JSX.Element => (
  <div class={styles.container}>
    <DemoForm />
    <DemoIo
      style={{
        // eslint-disable-next-line @typescript-eslint/naming-convention
        "max-width": "50%",
      }}
    />
  </div>
);
