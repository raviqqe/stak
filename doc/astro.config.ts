import { defineConfig } from "astro/config";
import prefetch from "@astrojs/prefetch";
import sitemap from "@astrojs/sitemap";
import starlight from "@astrojs/starlight";
import { readFile } from "node:fs/promises";
import { join, parse, relative } from "node:path";
import { glob } from "glob";

const exampleDirectory = "src/content/docs/examples";

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
          items: await Promise.all(
            (await glob(join(exampleDirectory, "/**/*.md"))).map(
              async (path) => {
                const parsed = parse(relative(exampleDirectory, path));

                return {
                  label:
                    (await readFile(path, "utf-8"))
                      .split("\n")
                      .find((line) => line.startsWith("# "))
                      ?.replace("# ", "") ?? "",
                  link: join("examples", parsed.dir, parsed.name),
                };
              },
            ),
          ),
        },
      ],
    }),
  ],
  site: "https://raviqqe.github.io/stak",
});
