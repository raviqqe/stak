import { defineConfig } from "astro/config";
import prefetch from "@astrojs/prefetch";
import sitemap from "@astrojs/sitemap";

export default defineConfig({
  base: "/stak",
  image: {
    service: { entrypoint: "astro/assets/services/sharp" },
    remotePatterns: [{ protocol: "https" }],
  },
  integrations: [prefetch({ selector: "a", intentSelector: "a" }), sitemap()],
  site: "https://raviqqe.github.io/stak",
});
