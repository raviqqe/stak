import { atom, computed, task } from "nanostores";
import { init, Wasmer } from "@wasmer/sdk";

const initialization = computed(atom(), () => task(init));

const pkg = computed(initialization, () =>
  task(async () => {
    const response = await fetch("/stak/stak-repl.wasm");
    console.log(await response.text());
    return true
      ? Wasmer.fromRegistry("python/python")
      : Wasmer.fromFile(await response.bytes());
  }),
);

const instance = computed(pkg, (pkg) =>
  task(async () => pkg?.entrypoint?.run()),
);

export const stdin = computed(instance, (instance) => instance?.stdin);
export const stdout = computed(instance, (instance) => instance?.stdout);
