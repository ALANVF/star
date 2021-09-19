import parsing.Parser;
import lexing.Lexer;
import reporting.render.TextDiagnosticRenderer;
import reporting.Diagnostic;
import text.SourceFile;

@:publicFields @:structInit class Options {
	var buildDecls = true;
	var isStdlib = false;
	var pass1 = false;
	var callback: Null<(typing.Project) -> Void> = null;
}

class Main {
	static final stdout = Sys.stdout();
	public static final renderer = new TextDiagnosticRenderer(stdout);

	static inline function nl() {
		Sys.print("\n");
	}

	static inline function round(float: Float) {
		return Math.fround(float * 10000) / 10000;
	}

	static overload extern inline function testProject(path) _testProject(path, {});
	static overload extern inline function testProject(path, opt: Options) _testProject(path, opt);
	static function _testProject(path, opt: Options) {
		Sys.println('Path: $path');
		
		var time = 0.0;

		final startProject = haxe.Timer.stamp();
		final project = typing.Project.fromMainPath(path, !opt.isStdlib);
		final stopProject = haxe.Timer.stamp();
		final timeProject = round(stopProject*1000 - startProject*1000);
		time += timeProject * 10000;
		Sys.println('Gather sources time: ${timeProject}ms');

		final files = project.allFiles();
		
		final startSources = haxe.Timer.stamp();
		for(file in files) file.initSource();
		final stopSources = haxe.Timer.stamp();
		final timeSources = round(stopSources*1000 - startSources*1000);
		time += timeSources * 10000;
		Sys.println('Init sources time: ${timeSources}ms');

		final startParse = haxe.Timer.stamp();
		for(file in files) file.parse();
		final stopParse = haxe.Timer.stamp();
		final timeParse = round(stopParse*1000 - startParse*1000);
		time += timeParse * 10000;
		Sys.println('Parse sources time: ${timeParse}ms');

		final startImports = haxe.Timer.stamp();
		for(file in files) file.buildImports();
		final stopImports = haxe.Timer.stamp();
		final timeImports = round(stopImports*1000 - startImports*1000);
		time += timeImports * 10000;
		Sys.println('Build imports time: ${timeImports}ms');

		if(opt.buildDecls) {
			final startDecls = haxe.Timer.stamp();
			for(file in files) file.buildDecls();
			final stopDecls = haxe.Timer.stamp();
			final timeDecls = round(stopDecls*1000 - startDecls*1000);
			time += timeDecls * 10000;
			Sys.println('Build declarations time: ${timeDecls}ms');
		}

		if(opt.pass1) {
			final startPass1 = haxe.Timer.stamp();
			project.pass1();
			final stopPass1 = haxe.Timer.stamp();
			final timePass1 = round(stopPass1*1000 - startPass1*1000);
			time += timePass1 * 10000;
			Sys.println('Typer pass 1 time: ${timePass1}ms');
		}

		for(file in files) {
			for(i => err in file.allErrors()) {
				#if windows
					renderer.writer.cursor(MoveDown(1));
					renderer.writer.write("\033[G");
					renderer.writer.clearLine();
					Sys.sleep(0.05);
				#end
				renderer.render(err);
				#if windows
					renderer.writer.attr(RESET);
					Sys.sleep(0.05);
					renderer.writer.flush();
				#end

				if(i == 25) throw "too many errors!";
			}
		}

		Sys.println('Status: ${files.none(file -> file.hasErrors())}');
		Sys.println('Total time: ${time / 10000}ms');

		opt.callback._and(cb => {
			cb(project);
		});

		if(opt.isStdlib) {
			typing.Project.STDLIB = project;
		}
	}

	static function main() {
		#if windows
		util.HLSys.setFlags(util.HLSys.AUTO_FLUSH | util.HLSys.WIN_UTF8);
		#end

		/*Sys.println("=== EXAMPLES ===");

		for(file in allFiles("examples")) {
			parse(newSource(file), false);
		}*/

		testProject("stdlib", {
			isStdlib: true,
			pass1: true
		});/*(project: typing.Project) -> {
			project.findType(List2.of(["Star", []], ["Core", []], ["Array", []]), true).forEach(array -> {
				trace(array.simpleName());
			});
		});*/
		/*nl();
		testProject("tests/hello-world", {pass1: true});
		nl();
		testProject("tests/classes/point", {pass1: true});
		nl();
		testProject("tests/kinds", {pass1: true});
		nl();
		testProject("tests/aliases", {pass1: true} /*, (project: typing.Project) -> {
			project.findType(List2.of(["Direct", []]), true).forEach(direct -> {
				trace(direct.simpleName());
			});
		}* /);*/
		nl();
		testProject("star", {pass1: true});
		/*nl();
		testProject("tests/self", true, (project: typing.Project) -> {
			project.findType(List2.of(["Slot", []]), true).forEach(slot -> {
				trace(slot.simpleName());
			});

			project.findType(List2.of(["AST", []]), true).forEach(ast -> {
				trace(ast.simpleName());
				trace(ast.findType(List2.of(["Slot", []])).map(t -> t.simpleName()));
			});

			project.findType(List2.of(["AST", []], ["Slot", []]), true).forEach(slot -> {
				trace(slot.simpleName());
			});

			project.findType(List2.of(["AST", []], ["Expr", []]), true).forEach(expr -> {
				trace(expr.simpleName());
			});

			project.findType(List2.of(["AST", []], ["Slot", []], ["Method", []]), true).forEach(method -> {
				trace(method.simpleName());
				trace(method.findType(List2.of(["AST", []]), true).map(t -> t.simpleName()));
				trace(method.findType(List2.of(["AST", []], ["Slot", []]), true).map(t -> t.simpleName()));
				trace(method.findType(List2.of(["Slot", []]), true).map(t -> t.simpleName()));
			});
		});*/
		
		/*nl();
		for(s in new compiler.Compiler().stmts) Sys.println(s.form());
		
		compiler.nim.Compiler.test();*/
	}
}