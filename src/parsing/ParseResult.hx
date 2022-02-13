package parsing;

import lexing.Token;
import errors.Error;

enum ParseResult<T> {
	Success(made: T, rest: List<Token>);
	Failure(begin: List<Token>, end: Option<List<Token>>);
	Fatal(begin: List<Token>, end: Option<List<Token>>);
	Eof(begin: List<Token>);
	FatalError(error: Error);
}