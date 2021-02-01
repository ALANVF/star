import parsing.Parser;
import lexing.Lexer;
import reporting.render.TextDiagnosticRenderer;
import reporting.Diagnostic;
import text.SourceFile;

class Main {
	static final stdout = Sys.stdout();
	static final renderer = new TextDiagnosticRenderer(stdout);

	static function newSource(path) {
		return new SourceFile(path, sys.io.File.getContent(path));
	}

	static function parse(source: SourceFile, verbose = true) {
		Sys.println('\nFile:       ${source.path}');
		
		if(verbose) {
			Sys.println('Lines:      ${source.lineCount}');

			final size = sys.FileSystem.stat(source.path).size;
			Sys.println("Size:       " + if(size < 1000) '${size} bytes' else '${Math.fround(size / 100) / 10}kB');
		}

		final lexer = new Lexer(source);

		try {
			final startTokenize = haxe.Timer.stamp();
			lexer.tokenize();
			final stopTokenize = haxe.Timer.stamp();

			final tokens = lexer.tokens;

			final startParse = haxe.Timer.stamp();
			final program = Parser.parse(tokens);
			final stopParse = haxe.Timer.stamp();

			switch program {
				case Modular(errors, _):
					final status = errors.length == 0;
					final tokenizeTime = stopTokenize*1000 - startTokenize*1000;
					final parseTime = stopParse*1000 - startParse*1000;
	
					if(status) {
						Sys.println("Status:     \033[1;32mSuccess!\033[0m");
						if(verbose) {
							Sys.println('Tokenizing: ${tokenizeTime}ms');
							Sys.println('Parsing:    ${parseTime}ms');
						}
						Sys.println('Time:       ${tokenizeTime + parseTime}ms');
					} else {
						Sys.println("Status:     \033[1;31mFailure\033[0m");
	
						for(i => error in errors) {
							if(i == 25) {
								throw "TOO MANY ERRORS";
							}
	
							renderer.render(error);
						}
					}
				
				case Script(_, _): throw "todo!";
			}
		} catch(diag: Diagnostic) {
			renderer.render(diag);
			Sys.print("\n");
		}
	}

	static function allFiles(root): Array<String> {
		final files = [];

		for(path in sys.FileSystem.readDirectory(root)) {
			final fullPath = '$root/$path';
			
			if(sys.FileSystem.isDirectory(fullPath)) {
				for(file in allFiles(fullPath)) files.push(file);
			} else {
				files.push(fullPath);
			}
		}

		return files;
	}

	static function main() {
		Sys.println("=== EXAMPLES ===");

		for(file in allFiles("examples")) {
			parse(newSource(file), false);
		}

		Sys.println("\n\n=== STDLIB ===");

		for(file in allFiles("stdlib")) {
			parse(newSource(file), false);
		}
	}
}