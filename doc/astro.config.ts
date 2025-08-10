import { readdir, readFile, stat } from "node:fs/promises";
import { join, parse } from "node:path";
import sitemap from "@astrojs/sitemap";
import solid from "@astrojs/solid-js";
import starlight from "@astrojs/starlight";
import { defineConfig } from "astro/config";
import { capitalize, sortBy } from "es-toolkit";
import wasm from "vite-plugin-wasm";

type Item = { label: string; link: string } | { label: string; items: Item[] };

const documentDirectory = "src/content/docs";

const listItems = async (directory: string): Promise<Item[]> =>
  sortBy(
    await Promise.all(
      (await readdir(join(documentDirectory, directory)))
        .filter((path) => !path.startsWith("."))
        .map(async (path) => {
          const fullPath = join(documentDirectory, directory, path);
          const { name } = parse(path);
          const linkPath = join(directory, name);

          return (await stat(fullPath)).isDirectory()
            ? {
                items: await listItems(linkPath),
                label: capitalize(name.replace("-", " ")),
              }
            : {
                label:
                  (await readFile(fullPath, "utf-8"))
                    .split("\n")
                    .find((line) => line.startsWith("title: "))
                    ?.replace("title: ", "")
                    .trim() ?? "",
                link: linkPath,
              };
        }),
    ),
    [({ label, link }) => [!link, label]],
  );

export default defineConfig({
  base: "/stak",
  integrations: [
    sitemap(),
    solid(),
    starlight({
      customCss: ["./src/index.css"],
      favicon: "/icon.svg",
      head: [
        {
          attrs: {
            href: "/stak/manifest.json",
            rel: "manifest",
          },
          tag: "link",
        },
        {
          attrs: {
            content: "/stak/icon.svg",
            property: "og:image",
          },
          tag: "meta",
        },
        {
          attrs: {
            "data-domain": "raviqqe.com",
            defer: true,
            src: "https://plausible.io/js/plausible.js",
          },
          tag: "script",
        },
      ],
      logo: {
        src: "./public/icon.svg",
      },
      sidebar: [
        {
          label: "Home",
          link: "/",
        },
        {
          items: [
            {
              label: "Install",
              link: "/install",
            },
            {
              label: "Embedding Scheme in Rust",
              link: "/embedding-scripts",
            },
            {
              label: "Running in no-std and no-alloc environment",
              link: "/no-std-no-alloc",
            },
            {
              label: "Hot reloading",
              link: "/hot-reload",
            },
            {
              label: "Writing a Scheme subset",
              link: "/writing-scheme-subset",
            },
          ],
          label: "Guides",
        },
        {
          label: "Limitations",
          link: "/limitations",
        },
        {
          items: [
            {
              label: "Interpreter",
              link: "/demo/interpreter",
            },
            {
              label: "Compiler",
              link: "/demo/compiler",
            },
          ],
          label: "Demo",
        },

        {
          items: await listItems("examples"),
          label: "Examples",
        },
      ],
      social: [
        {
          href: "https://github.com/raviqqe/stak",
          icon: "github",
          label: "GitHub",
        },
      ],
      title: "Stak Scheme",
    }),
  ],
  prefetch: { prefetchAll: true },
  redirects: {
    "/demo": "/interpreter-demo",
  },
  site: "https://raviqqe.com/stak",
  vite: {
    plugins: [wasm()],
    worker: {
      format: "es",
    },
  },
});
