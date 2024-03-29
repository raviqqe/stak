export interface SuccessResult<T> {
  value: T;
  error?: undefined;
}

export interface ErrorResult {
  error: string;
}

export type Result<T> = SuccessResult<T> | ErrorResult;
