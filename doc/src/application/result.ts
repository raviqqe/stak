interface ErrorResult {
  error: string;
}

export type Result<T> = ErrorResult | SuccessResult<T>;

interface SuccessResult<T> {
  error?: undefined;
  value: T;
}
