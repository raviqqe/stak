import { highlight } from "picolight";
import { scheme } from "picolight/languages/scheme";
import { githubDark } from "picolight/themes/github-dark";
import { githubLight } from "picolight/themes/github-light";

export const highlightScheme = (text: string): string =>
  highlight(
    text,
    scheme,
    document.querySelector("html")?.getAttribute("data-theme") === "dark"
      ? githubDark
      : githubLight,
  ).outerHTML;
