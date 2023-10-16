import { defineConfig } from "astro/config";
import prefetch from "@astrojs/prefetch";
import sitemap from "@astrojs/sitemap";
import starlight from "@astrojs/starlight";

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
      title: "My Docs",
      social: {
        github: "https://github.com/withastro/starlight",
      },
      sidebar: [
        {
          label: "Guides",
          items: [
            // Each item here is one entry in the navigation menu.
            { label: "Example Guide", link: "/guides/example/" },
          ],
        },
        {
          label: "Reference",
          autogenerate: { directory: "reference" },
        },
      ],
    }),
  ],
  site: "https://raviqqe.github.io/stak",
});
