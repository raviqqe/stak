import { atom, computed, task } from "nanostores";
import { init, Wasmer } from "@wasmer/sdk";

const initialization = computed(atom(), () => task(init));

const pkg = computed(initialization, () =>
  task(async () => Wasmer.fromRegistry("python/python")),
);

const instance = computed(pkg, (pkg) =>
  task(async () =>
    pkg?.entrypoint?.run({
      args: ["-c", "print('Hello, World!')"],
    }),
  ),
);

const { code, stdout } = await instance.wait();
console.log(`Python exited with ${code}: ${stdout}`);
