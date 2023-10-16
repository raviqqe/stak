import { defineConfig } from "astro/config";
import prefetch from "@astrojs/prefetch";
import sitemap from "@astrojs/sitemap";
import starlight from "@astrojs/starlight";
import { sortBy } from "lodash";
import { readFile, readdir } from "node:fs/promises";
import { join, parse, relative } from "node:path";

const documentDirectory = "src/content/docs";

const listItems = async (path: string) => {
  const directory = join(documentDirectory, path);

  return sortBy(
    await Promise.all(
      (await readdir(directory)).map(async (path) => {
        const parsed = parse(relative(directory, path));

        return {
          label:
            (await readFile(path, "utf-8"))
              .split("\n")
              .find((line) => line.startsWith("title: "))
              ?.replace("title: ", "")
              .trim() ?? "",
          link: join("examples", parsed.dir, parsed.name),
        };
      }),
    ),
    "label",
  );
};

export default defineConfig({
  base: "/stak",
  image: {
    service: { entrypoint: "astro/assets/services/sharp" },
    remotePatterns: [{ protocol: "https" }],
  },
  integrations: [
    prefetch({ selector: "a", intentSelector: "a" }),
    sitemap(),
    starlight({
      title: "Stak",
      social: {
        github: "https://github.com/raviqqe/stak",
      },
      sidebar: [
        {
          label: "Home",
          link: "/",
        },
        {
          label: "Examples",
          items: await listItems("examples"),
        },
      ],
    }),
  ],
  site: "https://raviqqe.github.io/stak",
});
