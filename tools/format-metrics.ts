import { argv, stdin, stdout } from "node:process";
import { createInterface } from "node:readline";

const [, , key, name] = argv;

const metrics = (await Array.fromAsync(createInterface({ input: stdin })))
	.filter(Boolean)
	.map((line) => {
		const [name, value] = line.split(" ");

		return {
			key: name.replaceAll("/", "_"),
			name,
			value: Number(value),
			unit: "bytes",
		};
	});

stdout.write(
	JSON.stringify({
		key,
		name,
		metrics,
		acceptables: [],
	}),
);
