interface Props {
  source: string;
  onSubmit: () => void;
}

export const DemoForm = ({ source, onSubmit }: Props) => {
  const foo = useState();
  return (
    <form class="container" onSubmit={onSubmit}>
      <textarea>{source}</textarea>
      <div>
        <button>Run</button>
      </div>
    </form>
  );
};
