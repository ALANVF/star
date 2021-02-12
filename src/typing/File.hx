package typing;

import reporting.Severity;
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
	var status: Bool;
	final imports: Array<Import>;
	final decls: Array<TypeDecl>;

	function new(dir, path, ?unit) {
		this.dir = dir;
		this.path = path;
		this.unit = Option.fromNull(unit);
		program = None;
		status = false;
		imports = [];
		decls = [];
	}

	function initSource() {
		source = new SourceFile(path, sys.io.File.getContent(path));
	}

	function parse() {
		try {
			final result = Parser.parse(new Lexer(source).tokenize());
			
			switch result {
				case Modular([], _) | Script([], _): status = true;
				case Modular(errors, _) | Script(errors, _): for(i => error in errors) {
					if(i == 25) {
						throw "Too many errors! Stopping...";
					}

					Main.renderer.render(error);
				}
			}

			program = Some(result);
		} catch(diag: Diagnostic) {
			Main.renderer.render(diag);
		}
	}

	function buildImports() {
		program.forEach(prog -> {
			final decls = switch prog {
				case Modular(_, decls2): decls2;
				case Script(_, decls2): decls2.filterMap(decl -> switch decl {
					case SDecl(decl2): decl2;
					default: null;
				});
			};
			var lastWasUse = true;

			for(decl in decls) switch decl {
				case DUse({span: span, kind: kind, generics: generics}):
					if(!lastWasUse) {
						lastWasUse = true;
						Main.renderer.render(new Diagnostic({
							severity: Severity.WARNING,
							message: "Unorganized code",
							info: [
								Spanned({
									span: span,
									message: "All imports should be at the beginning of the file",
									isSecondary: true
								})
							]
						}));
					}

					if(generics != Nil) {
						throw "NYI!";
					}

					imports.push(new Import({
						generics: [],
						span: span,
						imports: switch kind {
							case Import(One(imp)) | ImportFrom(One(imp), _, _): [imp];
							case Import(Many(_, imps, _)) | ImportFrom(Many(_, imps, _), _, _): imps.copy();
							case Pragma(span2, pragma):
								status = false;
								Main.renderer.render(new Diagnostic({
									severity: Severity.ERROR,
									message: "Unknown pragma",
									info: [
										Spanned({
											span: span2,
											message: 'Unknown pragma `$pragma`',
											isPrimary: true
										})
									]
								}));
								continue;
						},
						from: switch kind {
							case Import(_): None;
							case ImportFrom(_, _, from): Some(from);
							case Pragma(_, _): continue;
						}
					}));

				default: if(lastWasUse) lastWasUse = false;
			}
		});
	}
}