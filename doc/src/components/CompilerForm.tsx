import preact from "preact";

interface Props {}

export const DemoForm = ({}: Props) => (
  <form class="container" onsubmit="submit()">
    <textarea>{source}</textarea>
    <div>
      <button>Run</button>
    </div>
  </form>
);
