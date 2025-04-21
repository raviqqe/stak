import { atom, computed, task } from "nanostores";
import { init, Wasmer } from "@wasmer/sdk";

const initialization = computed(atom(), () => task(init));

const pkg = computed(initialization, () =>
  task(async () => Wasmer.fromRegistry("python/python")),
);

const instance = computed(pkg, (pkg) =>
  task(async () => pkg?.entrypoint?.run()),
);

export const stdin = computed(instance, (instance) => instance?.stdin);
export const stdout = computed(instance, (instance) => instance?.stdout);
