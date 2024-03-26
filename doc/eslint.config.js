import configurations from "@raviqqe/eslint-config";

export default [
  ...configurations,
  {
    rules: {
      "@typescript-eslint/triple-slash-reference": "off",
    },
  },
];
