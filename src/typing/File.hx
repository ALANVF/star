package typing;

import reporting.Diagnostic;
import parsing.Parser;
import lexing.Lexer;
import parsing.ast.Program;
import text.SourceFile;

@:build(util.Auto.build())
class File {
	final dir: Dir;
	final path: String;
	var unit: Option<Unit>;
	var source: SourceFile;
	var program: Option<Program>;
	final imports: Array<Import>;
	final decls: Array<TypeDecl>;

	function new(dir, path, ?unit) {
		this.dir = dir;
		this.path = path;
		this.unit = Option.fromNull(unit);
		program = None;
		imports = [];
		decls = [];
	}

	function initSource() {
		source = new SourceFile(path, sys.io.File.getContent(path));
	}

	function parse() {
		try {
			program = Some(Parser.parse(new Lexer(source).tokenize()));
		} catch(diag: Diagnostic) {
			Main.renderer.render(diag);
		}
	}
}