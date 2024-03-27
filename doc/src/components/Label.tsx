import { type ComponentChildren, type JSX } from "preact";

interface Props {
  children: ComponentChildren;
  for: string;
}

export const Label = ({ children, for: forAttribute }: Props): JSX.Element => (
  <label for={forAttribute}>{children}</label>
);
