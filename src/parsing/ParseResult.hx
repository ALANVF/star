package parsing;

import lexing.Token;
import reporting.Diagnostic;

@:using(ParseResult.ParseResultHelper)
enum ParseResult<T> {
	Success(made: T, rest: List<Token>);
	Failure(begin: List<Token>, end: Option<List<Token>>);
	Fatal(begin: List<Token>, end: Option<List<Token>>);
	Eof(begin: List<Token>);
	FatalError(error: Diagnostic);
}


@:noCompletion
class ParseResultHelper {
	
}