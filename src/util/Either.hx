package util;

enum Either<T, U> {
	Left(v: T);
	Right(v: U);
}