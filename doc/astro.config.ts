import { defineConfig } from "astro/config";
import prefetch from "@astrojs/prefetch";
import sitemap from "@astrojs/sitemap";
import starlight from "@astrojs/starlight";
import { sortBy, capitalize } from "lodash";
import { readFile, readdir, stat } from "node:fs/promises";
import { join, parse } from "node:path";

type Item = { label: string; link: string } | { label: string; items: Item[] };

const documentDirectory = "src/content/docs";

const listItems = async (directory: string): Promise<Item[]> => {
  return sortBy(
    await Promise.all(
      (await readdir(join(documentDirectory, directory)))
        .filter((path) => !path.startsWith("."))
        .map(async (path) => {
          const fullPath = join(documentDirectory, directory, path);
          const { name } = parse(path);
          path = join(directory, name);

          return (await stat(fullPath)).isDirectory()
            ? {
                label: capitalize(name.replace("-", " ")),
                items: await listItems(path),
              }
            : {
                label:
                  (await readFile(fullPath, "utf-8"))
                    .split("\n")
                    .find((line) => line.startsWith("title: "))
                    ?.replace("title: ", "")
                    .trim() ?? "",
                link: path,
              };
        }),
    ),
    ({ label, link }) => [!link, label],
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
      favicon: "/icon.svg",
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
