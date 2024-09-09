export interface SuccessResult<T> {
  error?: undefined;
  value: T;
}

export interface ErrorResult {
  error: string;
}

export type Result<T> = ErrorResult | SuccessResult<T>;
