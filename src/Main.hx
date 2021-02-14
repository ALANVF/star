import parsing.Parser;
import lexing.Lexer;
import reporting.render.TextDiagnosticRenderer;
import reporting.Diagnostic;
import text.SourceFile;

class Main {
	static final stdout = Sys.stdout();
	public static final renderer = new TextDiagnosticRenderer(stdout);

	static inline function nl() {
		Sys.print("\n");
	}

	/*static function newSource(path) {
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
	}*/

	static function testProject(path, buildDecls = false) {
		var time = 0.0;

		final startProject = haxe.Timer.stamp();
		final project = typing.Project.fromMainPath(path);
		final stopProject = haxe.Timer.stamp();
		final timeProject = stopProject*1000 - startProject*1000;
		time += timeProject;
		Sys.println('Gather sources time: ${timeProject}ms');

		final files = project.allFiles();
		
		final startSources = haxe.Timer.stamp();
		for(file in files) file.initSource();
		final stopSources = haxe.Timer.stamp();
		final timeSources = stopSources*1000 - startSources*1000;
		time += timeSources;
		Sys.println('Init sources time: ${timeSources}ms');

		final startParse = haxe.Timer.stamp();
		for(file in files) file.parse();
		final stopParse = haxe.Timer.stamp();
		final timeParse = stopParse*1000 - startParse*1000;
		time += timeParse;
		Sys.println('Parse sources time: ${timeParse}ms');

		final startImports = haxe.Timer.stamp();
		for(file in files) file.buildImports();
		final stopImports = haxe.Timer.stamp();
		final timeImports = stopImports*1000 - startImports*1000;
		time += timeImports;
		Sys.println('Build imports time: ${timeImports}ms');

		if(buildDecls) {
			final startDecls = haxe.Timer.stamp();
			for(file in files) file.buildDecls();
			final stopDecls = haxe.Timer.stamp();
			final timeDecls = stopDecls*1000 - startDecls*1000;
			time += timeDecls;
			Sys.println('Build declarations time: ${timeDecls}ms');
		}

		for(file in files) {
			file.allErrors().forEach(renderer.render);
		}

		Sys.println('Status: ${files.none(file -> file.hasErrors())}');
		Sys.println('Total time: ${time}ms');
	}

	static function main() {
		/*Sys.println("=== EXAMPLES ===");

		for(file in allFiles("examples")) {
			parse(newSource(file), false);
		}

		Sys.println("\n\n=== STDLIB ===");

		for(file in allFiles("stdlib")) {
			parse(newSource(file), false);
		}*/

		testProject("stdlib", true);
		nl();
		testProject("tests/hello-world", true);
		nl();
		testProject("tests/classes/point", true);
		nl();
		testProject("tests/kinds", true);
		nl();
		testProject("tests/aliases", true);
	}
}