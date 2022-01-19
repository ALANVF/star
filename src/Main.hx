import parsing.Parser;
import lexing.Lexer;
import reporting.render.TextDiagnosticRenderer;
import reporting.Diagnostic;
import text.SourceFile;

import parsing.ast.Expr;
import parsing.ast.Cascade;
import parsing.ast.Message;

@:publicFields @:structInit class Options {
	var buildDecls = true;
	var isStdlib = false;
	var pass1 = false;
	var pass2 = false;
	var callback: Null<(typing.Project) -> Void> = null;
}

class Main {
	static final stdout = Sys.stdout();
	public static final renderer = new TextDiagnosticRenderer(stdout);

	static var typesDump: haxe.io.Output = null;
	public static var typesDumper: typing.Dumper = null;
	static var stmtsDump: haxe.io.Output = null;
	public static var stmtsDumper: typing.Dumper = null;

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

		if(opt.pass1 && !files.some(f -> f.hasErrors())) {
			final startPass1 = haxe.Timer.stamp();
			project.pass1();
			final stopPass1 = haxe.Timer.stamp();
			final timePass1 = round(stopPass1*1000 - startPass1*1000);
			time += timePass1 * 10000;
			Sys.println('Typer pass 1 time: ${timePass1}ms');
		}

		if(opt.isStdlib) {
			typing.Pass2.initSTD(project);
		}

		if(opt.isStdlib) {
			typing.Project.STDLIB = project;
		}
		
		var err = null;
		if(opt.pass2 && !files.some(f -> f.hasErrors())) {
			final startPass2 = haxe.Timer.stamp();
			try typing.Pass2.resolveProject(project) catch(e: haxe.ValueException) err = e;
			final stopPass2 = haxe.Timer.stamp();
			final timePass2 = round(stopPass2*1000 - startPass2*1000);
			time += timePass2 * 10000;
			Sys.println('Typer pass 2 time: ${timePass2}ms');
		}

		err._and(e => Sys.println(e.details()));

		//if(!opt.isStdlib)
		for(file in files) {
			for(i => err in file.allErrors()) {
				#if windows
					renderer.writer.cursor(MoveDown(1));
					renderer.writer.write("\033[G");
					renderer.writer.clearLine();
					Sys.sleep(0.15);
				#end
				renderer.render(err);
				#if windows
					renderer.writer.attr(RESET);
					Sys.sleep(0.15);
					//renderer.writer.flush();
				#end

				if(i == 25) throw "too many errors!";
			}
		}
		
		Sys.println('Status: ${files.none(file -> file.hasErrors()) && err == null}');
		Sys.println('Total time: ${time / 10000}ms');

		opt.callback._and(cb => {
			cb(project);
		});
	}

	static function main() {
		#if windows
		util.HLSys.setFlags(util.HLSys.AUTO_FLUSH | util.HLSys.WIN_UTF8);
		#end

		/*Sys.println("=== EXAMPLES ===");

		for(file in allFiles("examples")) {
			parse(newSource(file), false);
		}*/
	
		final dumpTypes = false;
			
		if(dumpTypes) { typesDump = sys.io.File.write("./dump/types.stir"); typesDumper = new typing.Dumper(typesDump); }
		stmtsDump = sys.io.File.write("./dump/stmts.stir"); stmtsDumper = new typing.Dumper(stmtsDump);
		try {
			testProject("stdlib", {
				isStdlib: true,
				pass1: true,
				pass2: true
			});

			for(type in [
				typing.Pass2.STD_Int,
				typing.Pass2.STD_Dec,
				typing.Pass2.STD_Str
			].concat(typing.Pass2.STD_Array.t._match(
				at(TMulti(types)) => types,
				_ => throw "bad"
			)).concat(typing.Pass2.STD_Dict.t._match(
				at(TConcrete(decl)) => [typing.Pass2.STD_Dict],
				at(TMulti(types)) => types,
				_ => throw "bad"
			))) type.t._match(
				at(TConcrete(decl is typing.Class) | TModular({t: TConcrete(decl is typing.Class)}, _)) => {
					stmtsDumper.dump(decl);
					stmtsDumper.nextLine();
					stmtsDumper.nextLine();
				},
				_ => throw "???"+type
			);
			nl();
			testProject("star", {pass1: true, pass2: true});
		} catch(e: haxe.Exception) {
			if(dumpTypes) typesDump.close();
			stmtsDump.close();
			hl.Api.rethrow(e);
		}
		if(dumpTypes) typesDump.close();
		stmtsDump.close();

		/*function lex(text) {
			return new Lexer(new SourceFile("<anon>", text)).tokenize()._2;
		}
		function parse(tokens) {
			return (@:privateAccess Parser.parseFullExpr(tokens))._match(
				at(Success(made, Nil)) => made,
				_ => throw "???"
			);
		}
		function test(text) {
			final tokens = lex(text);
			final expr = parse(tokens);
			trace(expr);
			trace("");
			reparseStart(expr)._match(
				at(expr2!) => {
					trace(expr2);
				},
				_ => {
					trace("(no change)");
				}
			);
			nl();
		}

		test("$0 + 1");
		test("$0 + $1");
		test("$0.a");
		test("$0.a.b");
		test("$0.a + 1");
		test("$0[a: $.1]");
		test("[$0 a: $.1]");
		test("$0[a: $0 + $1]");
		test("[$0 a: $0 + $1]");
		test("$0[a: $0 + $.1]");
		test("$0.foo[bar: $.1] + $1[baz: $0 + $1.thing]");
		test("\" \\($.0)\"+1");
		test("foo[collect: \" \\($.0)\"]");
		test("foo[collect: \" \\($.0)\"][join]");*/
	}

	/*static function reparseStart(expr: Expr): Null<Expr> {
		return reparseInner(expr, Nil);
	}

	static function reparseInner(expr: Expr, stack: List<{n: Int}>): Null<Expr> {
		final stack2 = stack.prepend({n: 0});
		return reparse(expr, stack2)._and(
			expr2 => {
				stack2.head().n._match(
					at(0) => expr2,
					at(n) => EAnonFunc(stack2.length() - 1, n, expr2)
				);
			}
		);
	}

	static function reparseEach(exprs: Array<Expr>, stack: List<{n: Int}>): Null<Array<Expr>> {
		var hadAny = false;
		final exprs2 = exprs.map(e -> reparseInner(e, stack)._andOr(
			res => {
				if(!hadAny) hadAny = true;
				res;
			},
			e
		));
		
		return if(hadAny) exprs2 else null;
	}

	static function reparseMessage<T>(msg: Message<T>, stack: List<{n: Int}>): Null<Message<T>> {
		return msg._match(
			at(Multi(category, labels)) => {
				var hadAny = false;
				final labels2 = labels.map(l -> l._match(
					at(Named(span, name, expr)) => {
						reparseInner(expr, stack)._andOr(
							res => {
								if(!hadAny) hadAny = true;
								Label.Named(span, name, res);
							},
							l
						);
					},
					_ => l
				));

				if(hadAny) {
					Multi(category, labels2);
				} else {
					null;
				}
			},
			_ => null
		);
	}

	// !!! This mutates the cascades in order to be efficient !!!
	static function reparseCascades<T>(cascades: Array<Cascade<T>>, stack: List<{n: Int}>) {
		var hadAny = false;
		function loop<U>(cascade: Cascade<U>) {
			cascade.kind._match(
				at(Message(msg)) => {
					reparseMessage(msg, stack)._and(msg2 => {
						if(!hadAny) hadAny = true;
						cascade.kind = Message(msg2);
					});
				},
				at(AssignMember(mem, span, op, expr)) => {
					reparseInner(expr, stack)._and(expr2 => {
						if(!hadAny) hadAny = true;
						cascade.kind = AssignMember(mem, span, op, expr2);
					});
				},
				at(AssignMessage(msg, span, op, expr)) => {
					final msg2 = reparseMessage(msg, stack);
					final expr2 = reparseInner(expr, stack);
					if(msg2 != null && expr2 != null) {
						if(!hadAny) hadAny = true;
						cascade.kind = AssignMessage(msg2._or(msg), span, op, expr2._or(expr));
					}
				},
				at(StepMessage(msg, span, step)) => {
					reparseMessage(msg, stack)._and(msg2 => {
						if(!hadAny) hadAny = true;
						cascade.kind = StepMessage(msg2, span, step);
					});
				},
				at(Block(blk)) => {}, // TODO
				_ => {}
			);

			for(nested in cascade.nested) {
				loop(nested);
			}
		}

		for(cascade in cascades) {
			loop(cascade);
		}

		return hadAny;
	}

	static function reparse(expression: Expr, stack: List<{n: Int}>): Null<Expr> return expression._match(
		at(EStr(_, [] | [PStr(_)])) => null,
		at(EStr(span, parts)) => {
			var hadAny = false;
			final stack2 = stack.prepend({n: 0});
			final parts2 = parts.map(p -> p._match(
				at(PStr(_)) => p,
				at(PCode(code)) => reparse(code, stack2)._andOr(
					e => {
						if(!hadAny) hadAny = true;
						PCode(e);
					}, {
						p;
					}
				)
			));
			if(stack2.head().n > 0) throw "error: closures cannot be coerced to a string!";
			if(hadAny) {
				EStr(span, parts2);
			} else {
				null;
			}
		},

		at(EArray(_begin, values, _end)) => {
			reparseEach(values, stack)._and(values2 => {
				EArray(_begin, values2, _end);
			});
		},
		at(EHash(_begin, pairs, _end)) => {
			var hadAny = false;
			final pairs2 = pairs.map(p -> {
				var hadEither = false;
				final k = reparseInner(p.k, stack)._andOr(
					res => {
						hadEither = true;
						res;
					},
					p.k
				);
				final v = reparseInner(p.v, stack)._andOr(
					res => {
						hadEither = true;
						res;
					},
					p.v
				);

				if(!hadAny) hadAny = hadEither;
				if(hadEither) {k: k, v: v} else p;
			});
			
			if(hadAny) {
				EHash(_begin, pairs2, _end);
			} else {
				null;
			}
		},
		at(ETuple(_begin, values, _end)) => {
			reparseEach(values, stack)._and(values2 => {
				ETuple(_begin, values2, _end);
			});
		},
		
		at(EAnonArg(span, depth, nth)) => {
			final env = stack.nth(depth);
			env.n = env.n.max(nth + 1);
			final name = '${stack.length() - depth - 1}@$nth';
			EName(span, name);
		},

		at(ELiteralCtor(type, literal)) => {
			reparse(literal, stack)._andOr(
				lit => ELiteralCtor(type, lit),
				null
			);
		},

		at(EParen(_begin, exprs, _end)) => {
			reparseEach(exprs, stack)._and(exprs2 => {
				EParen(_begin, exprs2, _end);
			});
		},

		at(ETypeMessage(type, _begin, msg, _end)) => {
			reparseMessage(msg, stack)._and(msg2 => {
				ETypeMessage(type, _begin, msg2, _end);
			});
		},

		at(ETypeCascade(type, cascades)) => {
			final hadAny = reparseCascades(cascades, stack);

			if(hadAny) {
				expression;
			} else {
				null;
			}
		},

		at(EObjMessage(sender, _begin, msg, _end)) => {
			var hadSender = false;
			final exprSpan = sender.mainSpan();
			final sender2 = if(exprSpan.start < _begin.start) { // obj[...]
				reparse(sender, stack)._andOr(
					res => {
						hadSender = true;
						res;
					},
					sender
				);
			} else { // [obj ...]
				reparseInner(sender, stack)._andOr(
					res => {
						hadSender = true;
						res;
					},
					sender
				);
			};

			reparseMessage(msg, stack)._andOr(msg2 => {
				EObjMessage(sender2, _begin, msg2, _end);
			}, {
				if(hadSender) {
					EObjMessage(sender2, _begin, msg, _end);
				} else {
					null;
				}
			});
		},
		
		at(EObjCascade(sender, cascades)) => {
			var hadSender = false;
			final sender2 = reparse(sender, stack)._andOr(
				res => {
					hadSender = true;
					res;
				},
				sender
			);

			final hadAny = reparseCascades(cascades, stack);

			if(hadSender) {
				EObjCascade(sender2, cascades);
			} else if(hadAny) {
				expression;
			} else {
				null;
			}
		},
		
		at(EObjMember(expr, member)) => {
			reparse(expr, stack)._andOr(
				e => EObjMember(e, member),
				null
			);
		},

		at(EPrefix(span, op, right)) => {
			reparse(right, stack)._andOr(
				r => EPrefix(span, op, r),
				null
			);
		},

		at(ESuffix(left, span, op)) => {
			reparse(left, stack)._andOr(
				l => ESuffix(l, span, op),
				null
			);
		},

		at(EInfix(left, span, op, right)) => {
			final left2 = reparse(left, stack);
			final right2 = reparse(right, stack);
			if(left2 == null && right2 == null) {
				null;
			} else {
				EInfix(left2._or(left), span, op, right2._or(right));
			}
		},

		at(EVarDecl(span, name, type, Some(value))) => {
			reparseInner(value, stack)._andOr(
				value2 => EVarDecl(span, name, type, Some(value2)),
				null
			);
		},

		_ => null
	);*/
}