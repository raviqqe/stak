interface ErrorResult {
  error: string;
}

// TODO Use `T | Error`.
export type Result<T> = ErrorResult | SuccessResult<T>;

interface SuccessResult<T> {
  error?: undefined;
  value: T;
}
