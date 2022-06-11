package typing;

typedef DumpAttrs = Array<Either<String, () -> Void>>;

@:publicFields
@:build(util.Overload.build())
class Dumper {
	var out: haxe.io.Output;
	var level: Int;
	final tab: String;
	final tabs: Array<String>;
	
	function new(out: haxe.io.Output, tabWidth: Int = 4) {
		this.out = out;
		level = 0;
		tab = " ".repeat(tabWidth);
		tabs = [""];
	}

	inline function newline() out.writeByte('\n'.code);

	function indent() {
		if(tabs.length - 1 < level) {
			tabs.push(tabs.last() + tab);
		}

		out.writeString(tabs[level]);
	}

	inline function nextLine() {
		newline();
		indent();
	}

	function writeUTF8(char: Char) {
		if(char.isAscii()) {
			out.writeByte(char);
		} else {
			out.writeString(char.toString());
		}
	}

	overload inline function writeChar(char: Char) out.writeByte(char);
	overload inline function writeChar(char: Char, count: Int) {
		for(_ in 0...count) out.writeByte(char);
	}
	
	overload inline function write(str: String) out.writeString(str);
	overload inline function write(str: String, count: Int) {
		for(_ in 0...count) out.writeString(str);
	}
	
	overload function write(values: Array<String>) {
		for(value in values) write(value);
	}
	overload function write(values: Array<String>, sep: String) {
		values._for(i => value, {
			if(i != 0) write(sep);
			write(value);
		});
	}
	overload function write<T>(values: Array<T>, sep: String, func: (T) -> Void) {
		values._for(i => value, {
			if(i != 0) write(sep);
			func(value);
		});
	}

	overload function write<T, U, V>(values: List3<T, U, V>, sep: String, func: (T, U, V) -> Void) {
		var cur = values;
		while(true) values._match(
			at([[a, b, c], ...rest]) => {
				if(cur != values) write(sep);
				func(a, b, c);
				cur = rest;
			},
			at([]) => break
		);
	}


	overload inline function writeLines(values: Array<String>) writeLines(values, true);
	overload function writeLines(values: Array<String>, newLevel: Bool) {
		if(newLevel) level++;
		
		for(value in values) {
			nextLine();
			write(value);
		}

		if(newLevel) level--;
	}

	overload inline function writeLines<T>(values: Array<T>, func: (T) -> Void) writeLines(values, func, true);
	overload function writeLines<T>(values: Array<T>, func: (T) -> Void, newLevel: Bool) {
		if(newLevel) level++;
		
		for(value in values) {
			nextLine();
			func(value);
		}

		if(newLevel) level--;
	}

	overload inline function writeLines<T>(values: List<T>, func: (T) -> Void) writeLines(values, func, true);
	overload function writeLines<T>(values: List<T>, func: (T) -> Void, newLevel: Bool) {
		if(newLevel) level++;
		
		values.forEach(value -> {
			nextLine();
			func(value);
		});

		if(newLevel) level--;
	}


	inline function writeLine(func: () -> Void) {
		level++;
		nextLine();
		func();
		level--;
	}
	

	function getTypePath(lookup: ITypeLookup) {
		return lookup._match(
			at(decl is TypeDecl) => if(decl.refinees.length != 0) {
				final refinees = Type.mostSpecificBy(decl.refinees, d -> d.thisType);

				"#"+Type.getFullPath(decl).value()
				+ "/" + refinees.findIndex(ref -> ref.refinements.contains(decl))._match(
					at(-1) => throw "???",
					at(i) => Std.string(i+decl.refinees.filter(ref -> ref.refinements.contains(decl)).length)
				);
			} else {
				"#"+Type.getFullPath(decl).value();
			},
			_ => "#"+Type.getFullPath(lookup).value()
		);
	}

	overload function dump(lookup: LookupPath, cache: Cache) {
		write(
			lookup,
			" ",
			(_, name, params) -> {
				write(name);
				if(params.length > 0) {
					write(" [");
					write(params, " ", p -> dump(p, cache));
					write(")");
				}
			}
		);
	}
	
	overload function dump(type: Type, cache: Cache) type.t._match(
		at(TPath(depth, lookup, _)) => {
			write("(type ");
			if(depth > 0) write('$depth ');
			dump(lookup, cache);
			write(")");
		},
		at(TLookup(ty, lookup, _)) => {
			write("(type ");
			dump(ty, cache);
			write(" ");
			dump(lookup, cache);
			write(")");
		},
		at(TConcrete(decl)) => {
			write("(type ");
			write(getTypePath(decl));
			if(decl.params.length > 0 && !cache.contains(decl)) {
				cache += decl;
				write(" [");
				write(decl.params, " ", p -> dump(p, cache));
				write("]");
			}
			write(")");
		},
		at(TInstance(decl, params, ctx)) => {
			cache += decl;
			write("(type ");
			write(getTypePath(decl));
			write(" [");
			write(params, " ", p -> dump(p, cache));
			write("] ");
			dump(ctx, cache);
			write(")");
		},
		at(TThis(source)) => {
			write("(type :this ");
			write(getTypePath(source));
			write(")");
		},
		at(TBlank) => {
			write("(type _)");
		},
		at(TMulti(types)) => {
			write("(type [");
			writeLines(types, t -> dump(t, cache));
			nextLine();
			write("])");
		},
		at(TApplied(ty, params)) => {
			write("(type ");
			dump(ty, cache);
			write(" [");
			write(params, " ", p -> dump(p, cache));
			write("])");
		},
		at(TTypeVar(typevar)) => {
			write("(type :");
			write(typevar.name.name);
			write(" ");
			typevar.lookup._match(
				at(mth is CastMethod) => {
					if(cache.contains(mth)) {
						write("(type ...) :cast (type ...)");
					} else {
						cache += mth;
						write(getTypePath(mth.decl));
						write(" :cast ");
						dump(mth.type, cache);
					}
				},
				at(mth is AnyMethod) => {
					write(getTypePath(mth.decl)+' #'+mth.methodName());
				},
				at(mth is EmptyMethod) => {
					write(getTypePath(mth.decl)+' "'+mth.declName+'"');
				},
				at(cat is Category) => {
					if(cache.contains(cat)) {
						write("(category ...)");
					} else {
						cache += cat;
						write("(category ");
						dump(cat.path, cache);
						write(" for ");
						dump(cat.thisType, cache);
						write(")");
					}
				},
				_ => {
					write(getTypePath(typevar.lookup));
				}
			);
			write(")");
		},
		at(TModular(ty, _)) => dump(ty, cache)
	);

	overload function dump(ctx: TypeVarCtx, cache: Cache) {
		write("{");
		level++;
		for(tvar => subst in ctx) {
			nextLine();
			write(":");
			write(tvar.name.name);
			write(" ");
			dump(subst, cache);
		}
		level--;
		nextLine();
		write("}");
	}

	overload function dump(kind: SingleStaticKind) kind._match(
		at(SSMethod(mth)) => {
			mth.decl._match(at(cat is Category) => {
				write(":category ");
				dump(cat.path, Cache.empty + cat);
				/*if(cat.thisType.strictUnifyWithType(type) == null) {
					write(" :for ");
					dump(cat.thisType);
				}*/
				nextLine();
			}, _ => {});
			write(':single-method #${mth.methodName()}');
			/*if(!(mth.decl is Category) && mth.decl.strictUnifyWithType(type) == null) {
				write(" :from ");
				dump(mth.decl.thisType);
			}*/
		},
		at(SSMultiMethod(mth)) => {
			mth.decl._match(at(cat is Category) => {
				write(":category ");
				dump(cat.path, Cache.empty + cat);
				nextLine();
			}, _ => {});
			write(':multi-method #${mth.methodName()}');
		},
		at(SSInit(init)) => {
			init.decl._match(at(cat is Category) => {
				write(":category ");
				dump(cat.path, Cache.empty + cat);
				nextLine();
			}, _ => {});
			write(':single-init #${init.methodName()}');
		},
		at(SSMultiInit(init)) => {
			init.decl._match(at(cat is Category) => {
				write(":category ");
				dump(cat.path, Cache.empty + cat);
				nextLine();
			}, _ => {});
			write(':multi-init #${init.methodName()}');
		},
		at(SSMember(mem)) => {
			mem.decl._match(at(cat is Category) => {
				write(":category ");
				dump(cat.path, Cache.empty + cat);
				nextLine();
			}, _ => {});
			write(':get-member #${mem.name.name}');
		},
		at(SSTaggedCase(tcase)) => {
			write(':single-tagged-case #${tcase.name.name}');
		},
		at(SSTaggedCaseAlias(tcase)) => {
			write(':single-tagged-case-alias #${tcase.assoc.nonNull()._match(
				at(Single(_, _, name)) => name,
				_ => throw "bad"
			)}');
		},
		at(SSValueCase(vcase)) => {
			write(':value-case #${vcase.name.name}');
		},
		at(SSFromTypevar(tvar, name, getter, kind2)) => {
			write(":from ");
			dump(tvar.thisType, Nil);
			nextLine();
			dump(kind2);
		},
		at(SSFromParent(parent, kind2)) => {
			write(":from ");
			dump(parent, Nil);
			nextLine();
			dump(kind2);
		}
	);

	overload function dump(kind: MultiStaticKind) kind._match(
		at(MSMethod(mth, partial)) => {
			mth.decl._match(at(cat is Category) => {
				write(":category ");
				dump(cat.path, Cache.empty + cat);
				nextLine();
			}, _ => {});
			partial._and(p => {
				write(':partial $p');
				nextLine();
			});
			write(':multi-method #${mth.methodName()}');
		},
		at(MSInit(init, partial)) => {
			init.decl._match(at(cat is Category) => {
				write(":category ");
				dump(cat.path, Cache.empty + cat);
				nextLine();
			}, _ => {});
			partial._and(p => {
				write(':partial $p');
				nextLine();
			});
			write(':multi-init #${init.methodName()}');
			write(" :from ");
			dump(init.decl.thisType, Nil);
		},
		at(MSMemberwiseInit(ms)) => {
			write(":memberwise-init [");
			write(ms, " ", m -> write("#"+m.name.name+":"));
			write("]");
		},
		at(MSMember(mem)) => {
			mem.decl._match(at(cat is Category) => {
				write(":category ");
				dump(cat.path, Cache.empty + cat);
				nextLine();
			}, _ => {});
			write(':member #${mem.name.name}:');
		},
		at(MSTaggedCase(ms1, tcase, ms2, partial)) => {
			if(ms1.length > 0 || ms2.length > 0) {
				write(":members [");
				write(ms1, " ", m -> write("#"+m.name.name+":"));
				write(ms2, " ", m -> write("#"+m.name.name+":"));
				write("]");
				nextLine();
			}
			partial._and(p => {
				write(':partial $p');
				nextLine();
			});
			write(':multi-tagged-case #');
			for(p in tcase.params) write(p.label.name+":");
		},
		at(MSTaggedCaseAlias(tcase)) => {
			write(':multi-tagged-case-alias #');
			tcase.assoc.nonNull()._match(
				at(Multi(_, labels)) => for(l in labels) write(l._match(
					at(Named(_, name, _) | Punned(_, name)) => name+":",
					at(Anon(_)) => "_:"
				)),
				_ => throw "bad"
			);
		},
		at(MSFromTypevar(tvar, names, setter, kind2)) => {
			write(":from ");
			dump(tvar.thisType, Nil);
			nextLine();
			dump(kind2);
		},
		at(MSFromParent(parent, kind2)) => {
			write(":from ");
			dump(parent, Nil);
			nextLine();
			dump(kind2);
		}
	);


	overload function dump(kind: SingleInstKind) kind._match(
		at(SIMethod(mth)) => {
			mth.decl._match(at(cat is Category) => {
				write(":category ");
				dump(cat.path, Cache.empty + cat);
				nextLine();
			}, _ => {});
			write(':single-method #${mth.methodName()}');
		},
		at(SIMultiMethod(mth)) => {
			mth.decl._match(at(cat is Category) => {
				write(":category ");
				dump(cat.path, Cache.empty + cat);
				nextLine();
			}, _ => {});
			write(':multi-method #${mth.methodName()}');
		},
		at(SIMember(mem)) => {
			mem.decl._match(at(cat is Category) => {
				write(":category ");
				dump(cat.path, Cache.empty + cat);
				nextLine();
			}, _ => {});
			write(':get-member #${mem.name.name}');
		},
		at(SIFromTypevar(tvar, name, getter, kind2)) => {
			write(":from ");
			dump(tvar.thisType, Nil);
			nextLine();
			dump(kind2);
		},
		at(SIFromParent(parent, kind2)) => {
			write(":from ");
			dump(parent, Nil);
			nextLine();
			dump(kind2);
		}
	);

	overload function dump(kind: MultiInstKind) kind._match(
		at(MIMethod(mth, partial)) => {
			mth.decl._match(at(cat is Category) => {
				write(":category ");
				dump(cat.path, Cache.empty + cat);
				nextLine();
			}, _ => {});
			partial._and(p => {
				write(':partial $p');
				nextLine();
			});
			write(':multi-method #${mth.methodName()}');
			write(" :from ");
			dump(mth.decl.thisType, Nil);
		},
		at(MIMember(mem)) => {
			mem.decl._match(at(cat is Category) => {
				write(":category ");
				dump(cat.path, Cache.empty + cat);
				nextLine();
			}, _ => {});
			write(':member #${mem.name.name}:');
		},
		at(MIFromTypevar(tvar, names, setter, kind2)) => {
			write(":from ");
			dump(tvar.thisType, Nil);
			nextLine();
			dump(kind2);
		},
		at(MIFromParent(parent, kind2)) => {
			write(":from ");
			dump(parent, Nil);
			nextLine();
			dump(kind2);
		}
	);

	overload function dump(kind: CastKind) kind._match(
		at(CMethod(mth, ctx)) => {
			var cache = Cache.empty;
			mth.decl._match(at(cat is Category) => {
				write(":category ");
				cache += cat;
				dump(cat.path, cache);
				nextLine();
			}, _ => {});
			write(":cast-method ");
			dump(mth.type, cache);
			ctx._and(tctx => if(tctx.size() > 0) {
				write(" ");
				dump(tctx, cache);
			});
		},
		at(CUpcast(parent)) => {
			write(":upcast ");
			dump(parent, Nil);
		},
		at(CDowncast(child)) => {
			write(":downcast ");
			dump(child, Nil);
		},
		at(CNative(type)) => {
			write(":native-cast ");
			dump(type, Nil);
		},
		at(CFromTypevar(tvar, target, kind2)) => {
			write(":from ");
			dump(tvar.thisType, Nil);
			nextLine();
			dump(kind2);
		}
	);

	overload function dump(kind: UnaryOpKind) kind._match(
		at(UOMethod(mth)) => {
			write(":unary #");
			write(mth.methodName());
		},
		at(UOFromTypevar(tvar, op, kind2)) => {
			write(":from ");
			dump(tvar.thisType, Nil);
			nextLine();
			dump(kind2);
		}
	);

	overload function dump(kind: BinaryOpKind) kind._match(
		at(BOMethod(mth)) => {
			write(":binary #");
			write(mth.methodName());
			write(" :from ");
			dump(mth.decl.thisType, Nil);
		},
		at(BOFromTypevar(tvar, op, kind2)) => {
			write(":from ");
			dump(tvar.thisType, Nil);
			nextLine();
			dump(kind2);
		}
	);

	overload function dump(msg: TypeMessage) msg._match(
		at(Single(kind)) => {
			dump(kind);
			nextLine();
			write(kind.name());
		},
		at(Multi([c], labels, args)) => {
			write(":multi { ");
			level++;
			dump(c.kind);
			c.tctx._and(tctx => if(tctx.size() > 0) {
				write(" ");
				dump(tctx, Nil);
			});
			level--;
			write(" }");
			labels._for(i => label, {
				nextLine();
				write('$label: ');
				dump(args[i]);
			});
		},
		at(Multi(candidates, labels, args)) => {
			write(":multi [");
			writeLines(candidates, c -> {
				write("{ ");
				level++;
				dump(c.kind);
				c.tctx._and(tctx => if(tctx.size() > 0) {
					write(" ");
					dump(tctx, Nil);
				});
				level--;
				write(" }");
			});
			write("]");
			labels._for(i => label, {
				nextLine();
				write('$label: ');
				dump(args[i]);
			});
		},
		at(Super(parent, msg2)) => {
			write(":super ");
			dump(parent, Nil);
			nextLine();
			dump(msg2);
		}
	);

	overload function dump(msg: ObjMessage) msg._match(
		at(Lazy(msg2)) => dump(msg2),
		at(Single(kind)) => {
			dump(kind);
			nextLine();
			write(kind.name());
		},
		at(Multi([c], labels, args)) => {
			write(":multi { ");
			level++;
			dump(c.kind);
			c.tctx._and(tctx => if(tctx.size() > 0) {
				write(" ");
				dump(tctx, Nil);
			});
			level--;
			write(" }");
			labels._for(i => label, {
				nextLine();
				write('$label: ');
				if(args[i]==null) throw '${labels[0]}: ${args[0].orig.mainSpan().display()} $label: $i';
				dump(args[i]);
			});
		},
		at(Multi(candidates, labels, args)) => {
			write(":multi [");
			writeLines(candidates, c -> {
				write("{ ");
				level++;
				dump(c.kind);
				c.tctx._and(tctx => if(tctx.size() > 0) {
					write(" ");
					dump(tctx, Nil);
				});
				level--;
				write(" }");
			});
			write("]");
			labels._for(i => label, {
				nextLine();
				write('$label: ');
				dump(args[i]);
			});
		},
		at(Cast(target, [c])) => {
			write(":cast { ");
			level++;
			dump(c);
			level--;
			write(" }");
		},
		at(Cast(target, candidates)) => {
			write(":cast [");
			writeLines(candidates, c -> {
				write("{ ");
				level++;
				dump(c);
				level--;
				write(" }");
			});
			write("]");
		},
		at(Super(parent, msg2)) => {
			write(":super ");
			dump(parent, Nil);
			nextLine();
			dump(msg2);
		}
	);

	overload function dump<T>(msg: Message<T>) msg._match(
		at(Single(cat, name)) => {
			cat._and(c => {
				write(":category ");
				dump(c, Nil);
				nextLine();
			});
			write(":lazy");
			nextLine();
			write(name);
		},
		at(Multi(cat, labels, exprs)) => {
			cat._and(c => {
				write(":category ");
				dump(c, Nil);
				nextLine();
			});
			write(":lazy");
			labels._for(i => label, {
				nextLine();
				write('$label: ');
				dump(exprs[i]);
			});
		},
		at(Cast(cat, type)) => {
			cat._and(c => {
				write(":category ");
				dump(c, Nil);
				nextLine();
			});
			write(":lazy");
			nextLine();
			dump(type, cat._andOr(c => Cache.empty + c, Cache.empty));
		}
	);

	overload function dump<T>(cascades: Array<Cascade<T>>) {
		write("[");
		writeLines(cascades, cascade -> {
			dump(cascade.kind);
			if(cascade.nested.length > 0) {
				write(" ");
				dump(cascade.nested);
			}
		});
		write("]");
	}

	overload function dump(step: Cascade.Step) step._match(
		at(Incr) => write("++"),
		at(Decr) => write("--")
	);

	overload function dump<T>(kind: Cascade.CascadeKind<T>) kind._match(
		at(Member(name)) => write('(get $name)'),

		at(Message(msg)) => {
			write("(send { ");
			level++;
			dump(msg);
			level--;
			write(" })");
		},

		at(AssignMember(name, null, expr)) => {
			write('(set $name ');
			dump(expr);
			write(")");
		},
		at(AssignMember(name, op!!, expr)) => {
			write('(set $name :in-place #${op.symbol()} ');
			dump(expr);
			write(")");
		},

		at(AssignMessage(msg, null, expr)) => {
			write("(send { ");
			dump(msg);
			write(" } ");
			dump(expr);
			write(")");
		},
		at(AssignMessage(msg, op!!, expr)) => {
			write("(send { ");
			dump(msg);
			write(' } :in-place #${op.symbol()} ');
			dump(expr);
			write(")");
		},

		at(StepMember(name, step)) => {
			write('(set $name :in-place #');
			dump(step);
			write(")");
		},

		at(StepMessage(msg, step)) => {
			write("(send { ");
			dump(msg);
			write(" } :in-place #");
			dump(step);
			write(")");
		},
		
		at(Block(stmts)) => {
			write("(block");
			writeLines(stmts, s -> dump(s));
			write(")");
		}
	);

	overload function dump(cascades: Array<TypeCascade>) {
		write("[");
		writeLines(cascades, cascade -> {
			dump(cascade.kind);
			if(cascade.nested.length > 0) {
				write(" -> ");
				dump(cascade.nested);
			}
		});
		write("]");
	}

	overload function dump(kind: TypeCascade.TypeCascadeKind) kind._match(
		at(Lazy(kind2)) => {
			write("(lazy ");
			dump(kind2);
			write(")");
		},

		at(Member(msg) | Message(msg)) => {
			write("(send");
			level++;
			nextLine();
			dump(msg);
			level--;
			write(")");
		},

		at(AssignMember(setMsg, _, _) | AssignMessage(setMsg, _, _)) => {
			write("(send");
			level++;
			nextLine();
			dump(setMsg);
			level--;
			write(")");
		},

		at(StepMember(setMsg, _, step) | StepMessage(setMsg, _, step)) => {
			write("(send");
			level++;
			nextLine();
			dump(setMsg);
			nextLine();
			write(step.digForMethod().op.symbol()); // TODO
			level--;
			write(")");
		},

		at(Block(_, stmts)) => {
			write("(block");
			writeLines(stmts, s -> dump(s));
			write(")");
		}
	);

	overload function dump(cascades: Array<ObjCascade>) {
		write("[");
		writeLines(cascades, cascade -> {
			dump(cascade.kind);
			if(cascade.nested.length > 0) {
				write(" -> ");
				dump(cascade.nested);
			}
		});
		write("]");
	}

	overload function dump(kind: ObjCascade.ObjCascadeKind) kind._match(
		at(Lazy(kind2)) => {
			write("(lazy ");
			dump(kind2);
			write(")");
		},

		at(Member(msg) | Message(msg)) => {
			write("(send");
			level++;
			nextLine();
			dump(msg);
			level--;
			write(")");
		},

		at(AssignMember(setMsg, _, _) | AssignMessage(setMsg, _, _)) => {
			write("(send");
			level++;
			nextLine();
			dump(setMsg);
			level--;
			write(")");
		},

		at(StepMember(setMsg, _, step) | StepMessage(setMsg, _, step)) => {
			write("(send");
			level++;
			nextLine();
			dump(setMsg);
			nextLine();
			write(step.digForMethod().op.symbol()); // TODO
			level--;
			write(")");
		},

		at(Block(_, stmts)) => {
			write("(block");
			writeLines(stmts, s -> dump(s));
			write(")");
		}
	);

	overload function dump(expr: TExpr) {
		expr.e._match(
			at(TExpr.Expr.EInt(_, _) ... EBool(_)) => dump(expr.e),
			at(EVarDecl(_, null, null)) => dump(expr.e),
			_ => expr.t._match(
				at(type!) => {
					write("(typed");
					level++;
					nextLine();
					dump(type, Nil);
					nextLine();
					dump(expr.e);
					level--;
					write(")");
				},
				_ => {
					write("(untyped");
					level++;
					nextLine();
					dump(expr.e);
					level--;
					write(")");
				}
			)
		);
	}
	overload function dump(expression: TExpr.Expr) expression._match(
		at(EName(name, local)) => {
			// show orig ctx somehow?
			if(local is Pass2.LocalField) {
				write('(get-slot $name)');
			} else {
				write('(get $name)');
			}
		},

		at(ETag(tag, expr)) => {
			write('(tag #$tag ');
			dump(expr);
			write(")");
		},

		at(EInt(int, null)) => write('(int $int)'),
		at(EInt(int, exp!!)) => write('(int ${int}e$exp)'),

		at(EDec(int, dec, null)) => write('(dec $int.$dec)'),
		at(EDec(int, dec, exp!!)) => write('(dec $int.${dec}e$exp)'),

		at(EChar(char)) => {
			write('(char #"');
			write(char.escape());
			write('")');
		},

		at(EStr([])) => write('(str "")'),
		at(EStr(parts)) => {
			write("(str ");
			write(parts, " ", part -> part._match(
				at(PStr(str)) => {
					write('"');
					write(str.escape());
					write('"');
				},
				at(PCode(expr)) => {
					dump(expr);
				}
			));
			write(")");
		},

		at(EBool(true)) => write("(bool true)"),
		at(EBool(false)) => write("(bool false)"),

		at(EArray([])) => write("(array)"),
		at(EArray(values)) => {
			write("(array");
			writeLines(values, e -> dump(e));
			write(")");
		},

		at(EHash([])) => write("(dict)"),
		at(EHash(pairs)) => {
			write("(dict");
			writeLines(pairs, pair -> {
				dump(pair._1);
				write(" => ");
				dump(pair._2);
			});
			write(")");
		},

		at(ETuple([])) => write("(tuple)"),
		at(ETuple(values)) => {
			write("(tuple");
			writeLines(values, e -> dump(e));
			write(")");
		},

		at(EThis) => write("(this)"),
		
		at(EWildcard) => write("(_)"),

		at(EFunc(params, ret, body)) => {
			write("(func [");
			write(params, " ", param -> {
				write(param.name);
				param.type._and(type => {
					write(" ");
					dump(type, Nil);
				});
			});
			write("]");
			ret._and(ret_ => {
				write(" ");
				dump(ret_, Nil);
			});
			writeLines(body, s -> dump(s));
			write(")");
		},

		at(EAnonArg(depth, nth)) => {
			write('(anon-arg :depth $depth :nth $nth)');
		},

		at(ELiteralCtor(type, literal)) => {
			write("(ctor");
			level++;
			nextLine();
			dump(type, Nil);
			nextLine();
			dump(literal);
			level--;
			write(")");
		},
		
		at(EParen(exprs)) => {
			write("(paren");
			writeLines(exprs, e -> dump(e));
			write(")");
		},

		at(EBlock([])) => write("(block)"),
		at(EBlock(stmts)) => {
			write("(block");
			writeLines(stmts, s -> dump(s));
			write(")");
		},

		at(ETypeMessage(type, msg)) => {
			write("(send");
			level++;
			nextLine();
			dump(type, Nil);
			nextLine();
			dump(msg);
			level--;
			write(")");
		},
		at(ETypeCascade(type, cascades)) => {
			write("(cascade");
			level++;
			nextLine();
			dump(type, Nil);
			write(" -> ");
			dump(cascades);
			level--;
			write(")");
		},
		at(ETypeMember(type, kind)) => {
			write("(send");
			level++;
			nextLine();
			dump(type, Nil);
			nextLine();
			dump(kind);
			level--;
			write(")");
		},

		at(EObjMessage(expr, msg)) => {
			write("(send");
			level++;
			nextLine();
			dump(expr);
			nextLine();
			dump(msg);
			level--;
			write(")");
		},
		at(EObjCascade(expr, cascades)) => {
			write("(cascade");
			level++;
			nextLine();
			dump(expr);
			write(" -> ");
			dump(cascades);
			level--;
			write(")");
		},
		at(EObjLazyMember(expr, member)) => {
			write("(send");
			level++;
			nextLine();
			dump(expr);
			nextLine();
			write(':lazy-member #$member');
			level--;
			write(")");
		},
		at(EObjMember(expr, kind)) => {
			write("(send");
			level++;
			nextLine();
			dump(expr);
			nextLine();
			dump(kind);
			level--;
			write(")");
		},

		at(EPrefix(kind, right)) => {
			write("(send");
			level++;
			nextLine();
			dump(right);
			nextLine();
			write(":prefix");
			nextLine();
			dump(kind);
			level--;
			write(")");
		},
		at(ELazyPrefix(op, right)) => {
			write("(send");
			level++;
			nextLine();
			dump(right);
			nextLine();
			write(":lazy-prefix #");
			write(op.symbol());
			level--;
			write(")");
		},

		at(ESuffix(left, kind)) => {
			write("(send");
			level++;
			nextLine();
			dump(left);
			nextLine();
			write(":suffix");
			nextLine();
			dump(kind);
			level--;
			write(")");
		},
		at(ELazySuffix(left, op)) => {
			write("(send");
			level++;
			nextLine();
			dump(left);
			nextLine();
			write(":lazy-suffix #");
			write(op.symbol());
			level--;
			write(")");
		},

		at(EInfix(left, kinds, right)) => {
			write("(send");
			level++;
			nextLine();
			dump(left);
			nextLine();
			if(kinds.length == 1) {
				write(":infix { ");
				level++;
				final kind = kinds[0];
				dump(kind.kind);
				kind.tctx._and(tctx => if(tctx.size() > 0) {
					write(" ");
					dump(tctx, Nil);
				});
				level--;
				write(" }");
			} else {
				write(":infix [");
				writeLines(kinds, kind -> {
					write("{ ");
					level++;
					dump(kind.kind);
					kind.tctx._and(tctx => if(tctx.size() > 0) {
						write(" ");
						dump(tctx, Nil);
					});
					level--;
					write(" }");
				});
				write("]");
			}
			nextLine();
			dump(right);
			level--;
			write(")");
		},
		at(ELazyInfix(left, Assign(null), right)) => {
			write("(lazy-assign");
			level++;
			nextLine();
			dump(left);
			nextLine();
			dump(right);
			level--;
			write(")");
		},
		at(ELazyInfix(left, Assign(op!!), right)) => {
			write("(lazy-assign :in-place #"+op.symbol());
			level++;
			nextLine();
			dump(left);
			nextLine();
			dump(right);
			level--;
			write(")");
		},
		at(ELazyInfix(left, op, right)) => {
			write("(send");
			level++;
			nextLine();
			dump(left);
			nextLine();
			write(":lazy-infix #");
			write(op.symbol());
			nextLine();
			dump(right);
			level--;
			write(")");
		},
		at(EInfixChain(left, chain)) => {
			throw "how did this get here???";
		},

		at(EVarDecl(name, type, value)) => {
			write('(my $name');
			level++;
			type._and(ty => {
				nextLine();
				dump(ty, Nil);
			});
			value._and(val => {
				nextLine();
				dump(val);
			});
			level--;
			write(")");
		},

		at(EDestructure(pattern, value)) => {
			write('(destructure');
			level++;
			nextLine();
			dump(pattern);
			nextLine();
			dump(value);
			level--;
			write(")");
		},

		at(ESetName(name, local, value)) => {
			if(local is Pass2.LocalField) {
				write('(set-slot $name');
			} else {
				write('(set $name');
			}
			level++;
			nextLine();
			dump(value);
			level--;
			write(")");
		},

		at(EInitThis(type, msg)) => {
			write("(init-this");
			level++;
			nextLine();
			dump(msg);
			level--;
			write(")");
		},

		at(EInline(expr)) => {
			write("(inline" );
			dump(expr);
			write(")");
		},

		at(EKindId(expr)) => {
			write("(kind-id ");
			dump(expr);
			write(")");
		},
		at(EKindSlot(expr, i)) => {
			write("(kind-slot ");
			dump(expr);
			write(' $i)');
		},

		at(EInvalid) => {
			write("(!!!)");
		},

		at(EPatternType(type)) => {
			write("(pattern-type ");
			dump(type, Nil);
			write(")");
		}
	);

	overload function dump(pattern: Pattern) pattern.p._match(
		at(PExpr(expr)) => dump(expr),
		at(PExtractor(_)) => throw "NYI!",
		at(PExtractMessage(msg)) => {
			write("(extract-send");
			level++;
			nextLine();
			dump(msg);
			nextLine();
			dump(msg);
			level--;
			write(")");
		},
		at(PIgnore) => write("_"),
		at(PMy(name)) => {
			write("(my ");
			write(name);
			level++;
			nextLine();
			pattern.t._andOr(
				t => dump(t, Nil),
				write("???")
			);
			level--;
			write(")");
		},
		at(PMyType(name, type)) => {
			write("(my ");
			write(name);
			level++;
			nextLine();
			write(":flow ");
			dump(type, Nil);
			level--;
			write(")");
		},
		at(PType(type)) => {
			write("(of? ");
			dump(type, Nil);
			write(")");
		},
		at(PAll(patterns)) => {
			write("(all");
			writeLines(patterns, p -> dump(p));
			write(")");
		},
		at(PAny(patterns)) => {
			write("(any");
			writeLines(patterns, p -> dump(p));
			write(")");
		},
		at(POne(patterns)) => {
			write("(one");
			writeLines(patterns, p -> dump(p));
			write(")");
		},
		at(PNone(patterns)) => {
			write("(none");
			writeLines(patterns, p -> dump(p));
			write(")");
		},
		at(PNot(not)) => {
			write("(not");
			level++;
			nextLine();
			dump(not);
			level--;
			write(")");
		},
		at(PBoundsMin(min, _, pat)) => {
			write("(range");
			level++;
			nextLine();
			dump(pat);
			nextLine();
			write(":min ");
			dump(min);
			level--;
			write(")");
		},
		at(PBoundsMax(pat, max, _)) => {
			write("(range");
			level++;
			nextLine();
			dump(pat);
			nextLine();
			write(":max ");
			dump(max);
			level--;
			write(")");
		},
		at(PBoundsMinMax(min, _, pat, max, _)) => {
			write("(range");
			level++;
			nextLine();
			dump(pat);
			nextLine();
			write(":min ");
			dump(min);
			nextLine();
			write(":max ");
			dump(max);
			level--;
			write(")");
		},
		at(PAssignPattern(assign, pat)) => {
			write("(destructure");
			level++;
			nextLine();
			dump(assign);
			nextLine();
			dump(pat);
			level--;
			write(")");
		},
		at(PArray(patterns)) => {
			write("(array");
			writeLines(patterns, p -> p.p._match(
				at(PSpread(pat)) => {
					write(":spread ");
					dump(pat);
				},
				at(POptional(opt)) => {
					write(":opt");
					dump(opt);
				},
				_ => dump(p)
			));
			write(")");
		},
		at(PTypeArray(type, patterns)) => {
			write("(array :of ");
			dump(type, Nil);
			writeLines(patterns, p -> p.p._match(
				at(PSpread(pat)) => {
					write(":spread ");
					dump(pat);
				},
				at(POptional(opt)) => {
					write(":opt");
					dump(opt);
				},
				_ => dump(p)
			));
			write(")");
		},
		at(PTuple(patterns)) => {
			write("(tuple");
			writeLines(patterns, p -> dump(p));
			write(")");
		},
		at(PTypeTuple(type, patterns)) => {
			write("(tuple :of");
			dump(type, Nil);
			writeLines(patterns, p -> dump(p));
			write(")");
		},
		at(PTypeMembers(type, members)) => {
			write("(memberwise");
			level++;
			nextLine();
			dump(type, Nil);
			level--;
			writeLines(members, m -> {
				write(":"+m._1.name.name+" ");
				dump(m._2);
			});
			write(")");
		},
		at(PTypeValueCase(type, valueCase)) => {
			write("(value-kind");
			level++;
			nextLine();
			dump(type, Nil);
			nextLine();
			write("#"+valueCase.name.name);
			level--;
			write(")");
		},
		at(PTypeTaggedCaseSingle(type, taggedCase)) => {
			write("(tagged-kind");
			level++;
			nextLine();
			dump(type, Nil);
			nextLine();
			write(taggedCase.name.name);
			level--;
			write(")");
		},
		at(PTypeTaggedCaseMulti(type, taggedCase, args)) => {
			write("(tagged-kind");
			level++;
			nextLine();
			dump(type, Nil);
			taggedCase.params._for(i => param, {
				nextLine();
				write('${param.label.name}: ');
				dump(args[i]);
			});
			level--;
			write(")");
		},
		at(PTypeTaggedCaseMembersSingle(type, taggedCase, members)) => {
			write("(tagged-kind");
			level++;
			nextLine();
			dump(type, Nil);
			nextLine();
			write(taggedCase.name.name);
			nextLine();
			write(":members {");
			writeLines(members, m -> {
				write(":"+m._1+" ");
				dump(m._2);
			});
			write("}");
			level--;
			write(")");
		},
		at(PTypeTaggedCaseMembersMulti(type, taggedCase, args, members)) => {
			write("(tagged-kind");
			level++;
			nextLine();
			dump(type, Nil);
			taggedCase.params._for(i => param, {
				nextLine();
				write('${param.label.name}: ');
				dump(args[i]);
			});
			nextLine();
			write(":members {");
			writeLines(members, m -> {
				write(":"+m._1+" ");
				dump(m._2);
			});
			write("}");
			level--;
			write(")");
		},
		at(PExtractFrom(extract, from)) => {
			write("(extract");
			writeLines([extract, from], p -> dump(p));
			write(")");
		},
		at(PExcludeFrom(exclude, from)) => {
			write("(exclude");
			writeLines([exclude, from], p -> dump(p));
			write(")");
		},
		at(PComplement(pat)) => {
			write("(complement");
			level++;
			nextLine();
			dump(pat);
			level--;
			write(")");
		},
		_ => write("(!!!)")
	);

	overload function dump(statement: TStmt) statement.s._match(
		at(SExpr(expr)) => dump(expr.e),
		at(SIf(cond, then, orelse)) => {
			write("(if ");
			dump(cond);
			writeLines(then, s -> dump(s));
			orelse._and(oe => {
				nextLine();
				write("else");
				writeLines(oe, s -> dump(s));
			});
			write(")");
		},
		at(SCase(cases, orelse)) => {
			write("(case");
			writeLines(cases, c -> {
				write("(at ");
				dump(c.cond);
				writeLines(c.then, s -> dump(s));
				write(")");
			});
			orelse._and(oe => {
				level++;
				nextLine();
				write("(else");
				writeLines(oe, s -> dump(s));
				write(")");
				level--;
			});
			write(")");
		},
		at(SMatch(value, cases, orelse)) => {
			write("(match ");
			dump(value);
			writeLines(cases, c -> {
				write("(at ");
				dump(c.pattern);
				c.cond._and(cond => {
					write(" :if ");
					dump(cond);
				});
				writeLines(c.then, s -> dump(s));
				write(")");
			});
			orelse._and(oe => {
				level++;
				nextLine();
				write("(else");
				writeLines(oe, s -> dump(s));
				write(")");
				level--;
			});
			write(")");
		},
		at(SMatchAt(value, pattern, cond, then, orelse)) => {
			write("(match ");
			dump(value);
			level++;
			nextLine();
			write(":at ");
			dump(pattern);
			cond._and(c => {
				write(" :if ");
				dump(c);
			});
			level--;
			writeLines(then, s -> dump(s));
			orelse._and(oe => {
				nextLine();
				write("else");
				writeLines(oe, s -> dump(s));
			});
			write(")");
		},
		at(SWhile(cond, label, body)) => {
			write("(while ");
			dump(cond);
			label._and(lbl => {
				write(' :as "$lbl"');
			});
			writeLines(body, s -> dump(s));
			write(")");
		},
		at(SDoWhile(body, label, cond)) => {
			write("(do-while ");
			dump(cond);
			label._and(lbl => {
				write(' :as "$lbl"');
			});
			writeLines(body, s -> dump(s));
			write(")");
		},
		at(SForIn(lvar, lvar2, inExpr, cond, label, body)) => {
			write("(for ");
			lvar2._andOr(lv2 => {
				write("[");
				dump(lvar);
				write(" ");
				dump(lv2);
				write("]");
			}, {
				dump(lvar);
			});
			write(" :in ");
			dump(inExpr);
			cond._and(c => {
				write(" :while ");
				dump(cond);
			});
			label._and(lbl => {
				write(' :as "$lbl"');
			});
			writeLines(body, s -> dump(s));
			write(")");
		},
		at(SForRange(lvar, start, stop, by, cond, label, body)) => {
			write("(for ");
			lvar._and(lv => {
				dump(lv);
			});
			write(' :${start._1.getName().substr(4).toLowerCase()} ');
			dump(start._2);
			write(' :${stop._1.getName().substr(4).toLowerCase()} ');
			dump(stop._2);
			by._and(b => {
				write(" :by ");
				dump(b);
			});
			cond._and(c => {
				write(" :while ");
				dump(cond);
			});
			label._and(lbl => {
				write(' :as "$lbl"');
			});
			writeLines(body, s -> dump(s));
			write(")");
		},
		at(SDo(label, body)) => {
			write("(while ");
			label._and(lbl => {
				write(' :as "$lbl"');
			});
			writeLines(body, s -> dump(s));
			write(")");
		},
		at(SReturn(null)) => write("(return)"),
		at(SReturn(value!!)) => {
			write("(return ");
			dump(value);
			write(")");
		},
		at(SBreak(null)) => write("(break)"),
		at(SBreak(Left(depth))) => write('(break :depth $depth)'),
		at(SBreak(Right(label))) => write('(break :label "$label")'),
		at(SNext(null)) => write("(next)"),
		at(SNext(Left(depth))) => write('(next :depth $depth)'),
		at(SNext(Right(label))) => write('(next :label "$label")'),
		at(SThrow(span, value)) => {
			write("(throw ");
			dump(value);
			write(' :at "${span.display()}")');
		},
		at(STry(body, cases, orelse)) => {
			write("(try");
			writeLines(body, s -> dump(s));
			nextLine();
			write("catch");
			writeLines(cases, c -> {
				write("(at ");
				dump(c.pattern);
				c.cond._and(cond => {
					write(" :if ");
					dump(cond);
				});
				writeLines(c.then, s -> dump(s));
				write(")");
			});
			orelse._and(oe => {
				level++;
				nextLine();
				write("(else");
				writeLines(oe, s -> dump(s));
				write(")");
				level--;
			});
			write(")");
		}
	);

	
	overload function dump(attrs: DumpAttrs) {
		if(attrs.length > 0) {
			write(" is [");
			write(attrs, " ", attr -> attr._match(
				at(Left(name)) => write(name),
				at(Right(fn)) => fn()
			));
			write("]");
		}
	}

	overload function dump(typevars: MultiMap<String, TypeVar>) {
		if(typevars.size != 0) {
			write(" given [");
			final allTypevars = typevars.allValues();
			writeLines(allTypevars, tvar -> dump(tvar));
			if(allTypevars.length > 0) nextLine();
			write("]");
		}
	}

	overload function dump(member: Member) {
		write("(my ");
		write(member.name.name);
		member.type._and(type => {
			write(" ");
			dump(type, Nil);
		});

		final attrs: DumpAttrs = [];
		if(member.isReadonly) attrs.push(Left("readonly"));
		if(member.isStatic) attrs.push(Left("static"));
		member.hidden._and(hidden => {
			attrs.push(Right(() -> {
				hidden._match(
					at(Some(within)) => {
						write("hidden ");
						dump(within, Cache.empty /*+ member.lookup.thisType*/);
					},
					_ => {
						write("hidden");
					}
				);
			}));
		});
		if(member.noInherit) attrs.push(Left("noinherit"));
		member.getter._and(getter => {
			attrs.push(Right(() -> {
				getter._match(
					at(Some({name: name})) => write('getter "$name"'),
					at(None) => write("getter")
				);
			}));
		});
		member.setter._and(setter => {
			attrs.push(Right(() -> {
				setter._match(
					at(Some({name: name})) => write('setter "$name"'),
					at(None) => write("setter")
				);
			}));
		});
		dump(attrs);

		member.typedValue._and(tvalue => {
			write(" ");
			dump(tvalue);
		});

		write(")");
	}

	overload function dump(vcase: ValueCase) {
		write("(has "+vcase.name.name);
		vcase.typedValue._and(tvalue => {
			write(" ");
			dump(tvalue);
		});
		write(")");
	}

	overload function dump(tcase: TaggedCase) {
		write("(has [");
		
		tcase._match(
			at(mc is MultiTaggedCase) => {
				dump(mc.params, Nil);
			},
			at(sc is SingleTaggedCase) => {
				write(sc.name.name);
			},
			_ => throw "bad"
		);

		write("]");

		tcase.typedAssoc._and(assoc => {
			write(" => [");
			assoc._match(
				at(Single(null, name)) => {
					write(name);
				},
				at(Multi(null, labels, exprs)) => {
					labels._for(i => label, {
						nextLine();
						write('$label: ');
						dump(exprs[i]);
					});
				},
				_ => throw "error!"
			);
			write("]");
		});

		tcase.typedInit._and(init => {
			writeLines(init, s -> dump(s));
		});

		write(")");
	}

	overload function dump(method: EmptyMethod) {
		method._match(
			at(_ is DefaultInit) => write("(default-init"),
			at(_ is StaticInit) => write("(static-init"),
			at(_ is Deinit) => write("(deinit"),
			at(_ is StaticDeinit) => write("(static-deinit"),
			_ => throw "bad"
		);
		method.typedBody._and(body => writeLines(body, s -> dump(s)));
		write(")");
	}

	overload function dump(method: AnyMethod) {
		var cache = Cache.empty + method.decl.thisType;
		final attrs: DumpAttrs = [];
		var typevars: Null<MultiMap<String, TypeVar>> = null;
		var ret: Null<Type> = null;

		method.hidden._and(hidden => {
			attrs.push(Right(() -> {
				hidden._match(
					at(Some(within)) => {
						write("hidden ");
						dump(within, cache);
					},
					at(None) => {
						write("hidden");
					}
				);
			}));
		});
		if(method.noInherit) attrs.push(Left("noinherit"));
		method.native._and(native => {
			attrs.push(Right(() -> {
				native._match(
					at(Some(sym)) => write('native "${sym.name}"'),
					at(None) => write("native")
				);
			}));
		});
		if(method.isAsm) attrs.push(Left("asm"));

		method._match(
			at(oper is Operator) => {
				if(oper.isInline) attrs.push(Left("inline"));
				if(oper.isMacro) attrs.push(Left("macro"));
				ret = oper.ret;

				write('(operator #${oper.opName()}');
				oper._match(
					at(bo is BinaryOperator) => {
						typevars = bo.typevars;

						write(' [${bo.paramName.name} ');
						dump(bo.paramType, cache);
						write("]");
					},
					at(uo is UnaryOperator) => {},
					_ => throw "bad"
				);
			},
			at(init is Init) => {
				if(init.isMacro) attrs.push(Left("macro"));
				
				write("(init [");
				init._match(
					at(si is SingleInit) => {
						write(si.name.name);
					},
					at(mi is MultiInit) => {
						if(mi.isUnordered) attrs.push(Left("unordered"));
						typevars = mi.typevars;

						dump(mi.params, cache);
					},
					_ => throw "bad"
				);
				write("]");
			},
			at(mth is StaticMethod) => {
				if(!(mth.decl is Module)) attrs.push(Left("static"));
				if(mth.isMain) attrs.push(Left("main"));
				if(mth.isGetter) attrs.push(Left("getter"));
				if(mth.isSetter) attrs.push(Left("setter"));
				if(mth.isInline) attrs.push(Left("inline"));
				if(mth.isMacro) attrs.push(Left("macro"));
				ret = mth.ret;

				write("(on [");
				mth._match(
					at(sm is SingleStaticMethod) => {
						write(sm.name.name);
					},
					at(mm is MultiStaticMethod) => {
						if(mm.isUnordered) attrs.push(Left("unordered"));
						typevars = mm.typevars;

						dump(mm.params, cache);
					},
					_ => throw "bad"
				);
				write("]");
			},
			at(mth is Method) => {
				if(mth.isMain) attrs.push(Left("main"));
				if(mth.isGetter) attrs.push(Left("getter"));
				if(mth.isSetter) attrs.push(Left("setter"));
				if(mth.isInline) attrs.push(Left("inline"));
				if(mth.isMacro) attrs.push(Left("macro"));
				ret = mth.ret;

				write("(on [");
				mth._match(
					at(sm is SingleMethod) => {
						write(sm.name.name);
					},
					at(mm is MultiMethod) => {
						if(mm.isUnordered) attrs.push(Left("unordered"));
						typevars = mm.typevars;

						dump(mm.params, cache);
					},
					at(cm is CastMethod) => {
						typevars = cm.typevars;

						dump(cm.type, cache);
					},
					_ => throw "bad"
				);
				write("]");
			},
			_ => throw "bad"
		);

		ret._and(r => {
			write(" ");
			dump(r.simplify(), cache);
		});

		dump(attrs);

		typevars._and(tvars => dump(tvars));

		method.typedBody._and(body => {
			writeLines(body, s -> dump(s));
		});

		write(")");
	}

	overload function dump(params: MultiParams, cache: Cache) {
		writeLines(params, param -> {
			write(param.label.name+": "+param.name.name+" ");
			dump(param.type, cache);
			param.tvalue._and(tvalue => {
				write(" ");
				dump(tvalue);
			});
		});
		nextLine();
	}


	overload function dump(kind: NativeKind, cache: Cache) {
		write("(");
		kind._match(
			at(NVoid) => write("void"),
			at(NBool) => write("bool"),
			at(NInt8) => write("int8"),
			at(NUInt8) => write("uint8"),
			at(NInt16) => write("int16"),
			at(NUInt16) => write("uint16"),
			at(NInt32) => write("int32"),
			at(NUInt32) => write("uint32"),
			at(NInt64) => write("int64"),
			at(NUInt64) => write("uint64"),
			at(NDec32) => write("dec32"),
			at(NDec64) => write("dec64"),
			at(NVoidPtr) => write("voidptr"),
			at(NPtr(t)) => {
				write("ptr ");
				dump(t, cache);
			}
		);
		write(")");
	}

	function dumpParents(parents: Array<Type>, cache: Cache) {
		if(parents.length > 0) {
			write(" of [");
			write(parents, " ", t -> dump(t, cache));
			write("]");
		}
	}

	overload function dump(decl: AnyTypeDecl) {
		decl._match(
			at(tdecl is TypeDecl) => dump(tdecl),
			at(tvar is TypeVar) => dump(tvar),
			at(cat is Category) => dump(cat),
			_ => throw "bad"
		);
	}

	overload function dump(tvar: TypeVar) {
		var cache = Cache.empty + tvar.thisType;

		write("(typevar "+tvar.name.name);
		if(tvar.params.length > 0) {
			write(" [");
			write(tvar.params, " ", p -> dump(p, cache));
			write("]");
		}

		dumpParents(tvar.parents, cache);

		final attrs: DumpAttrs = [];
		if(tvar._isFlags) attrs.push(Left("flags"));
		if(tvar._isStrong) attrs.push(Left("strong"));
		if(tvar._isUncounted) attrs.push(Left("uncounted"));
		tvar.native._and(nat => {
			attrs.push(Right(() -> {
				write("native ");
				dump(nat, cache);
			}));
		});
		dump(attrs);
		tvar.rule._and(rule => {
			write(" if ");
			dump(rule, cache);
		});

		writeLines(tvar.valueCases, c -> dump(c));
		writeLines(tvar.taggedCases, c -> dump(c));
		writeLines(tvar.staticMembers, m -> dump(m));
		writeLines(tvar.members, m -> dump(m));
		writeLines(tvar.inits, i -> dump(i));
		writeLines(tvar.staticMethods, m -> dump(m));
		writeLines(tvar.methods, m -> dump(m));
		writeLines(tvar.operators, o -> dump(o));
		writeLines(tvar.categories, c -> dump(c));

		write(")");
	}

	overload function dump(rule: TypeRule, cache: Cache) {
		write("(");
		level++;
		rule._match(
			at(Eq(l, r)) => {
				write("eq");
				nextLine();
				dump(l, cache);
				nextLine();
				dump(r, cache);
			},
			at(Of(l, r)) => {
				write("of?");
				nextLine();
				dump(l, cache);
				nextLine();
				dump(r, cache);
			},
			at(Lt(l, r)) => {
				write("lt");
				nextLine();
				dump(l, cache);
				nextLine();
				dump(r, cache);
			},
			at(Le(l, r)) => {
				write("le");
				nextLine();
				dump(l, cache);
				nextLine();
				dump(r, cache);
			},
			at(All(rules)) => {
				write("all");
				writeLines(rules, r -> dump(r, cache), false);
			},
			at(Any(rules)) => {
				write("any");
				writeLines(rules, r -> dump(r, cache), false);
			},
			at(One(rules)) => {
				write("one");
				writeLines(rules, r -> dump(r, cache), false);
			},
			at(Not(r)) => {
				write("not");
				nextLine();
				dump(r, cache);
			},
			at(Negate(t)) => {
				write("negate");
				nextLine();
				dump(t, cache);
			},
			at(Exists(t)) => {
				write("exists?");
				nextLine();
				dump(t, cache);
			}
		);
		level--;
		write(")");
	}

	overload function dump(cat: Category) {
		var cache = Cache.empty;

		write("(category ");
		dump(cat.path, cache);
		cat.type._and(type => {
			write(" for ");
			dump(type, cache);
			
			cache += type;
		});
		cache += cat.path;

		final attrs: DumpAttrs = [];
		cat.hidden._and(hidden => {
			attrs.push(Right(() -> {
				hidden._match(
					at(Some(within)) => {
						write("hidden ");
						dump(within, cache);
					},
					at(None) => {
						write("hidden");
					}
				);
			}));
		});
		if(cat.friends.length > 0) {
			attrs.push(Right(() -> {
				if(cat.friends.length == 1) {
					write("friend ");
					dump(cat.friends[0], cache);
				} else {
					write("friend [");
					write(cat.friends, " ", f -> dump(f, cache));
					write("]");
				}
			}));
		}
		dump(attrs);

		dump(cat.typevars);

		cat.staticInit.toNull()._and(i => writeLine(() -> dump(i)));
		writeLines(cat.staticMembers, m -> dump(m));
		writeLines(cat.inits, i -> dump(i));
		writeLines(cat.staticMethods, m -> dump(m));
		writeLines(cat.methods, m -> dump(m));
		writeLines(cat.operators, o -> dump(o));
		cat.staticDeinit.toNull()._and(d => writeLine(() -> dump(d)));
	}

	overload function dump(tdecl: TypeDecl) {
		var cache = Cache.empty + tdecl.thisType;
		final attrs: DumpAttrs = [];
		if(tdecl.refinees.length > 0) {
			attrs.push(Left("refinement")); // TODO: list refinees
		}
		tdecl.hidden._and(hidden => {
			attrs.push(Right(() -> {
				hidden._match(
					at(Some(within)) => {
						write("hidden ");
						dump(within, cache);
					},
					at(None) => {
						write("hidden");
					}
				);
			}));
		});
		if(tdecl.friends.length > 0) {
			attrs.push(Right(() -> {
				if(tdecl.friends.length == 1) {
					write("friend ");
					dump(tdecl.friends[0], cache);
				} else {
					write("friend [");
					write(tdecl.friends, " ", f -> dump(f, cache));
					write("]");
				}
			}));
		}
		
		tdecl._match(
			at(decl is DirectAlias) => {
				write("(alias ");
				dumpName(decl, cache);
				
				dump(attrs);

				dump(decl.typevars);

				write(" ");
				dump(decl.type, cache);
			},
			at(decl is StrongAlias) => {
				write("(newtype ");
				dumpName(decl, cache);
				
				if(decl.noInherit) attrs.push(Left("noinherit"));
				dump(attrs);

				dump(decl.typevars);

				write(" ");
				dump(decl.type, cache);

				decl.staticInit.toNull()._and(i => writeLine(() -> dump(i)));
				writeLines(decl.staticMembers, m -> dump(m));
				writeLines(decl.members, m -> dump(m));
				writeLines(decl.staticMethods, m -> dump(m));
				writeLines(decl.methods, m -> dump(m));
				writeLines(decl.operators, o -> dump(o));
				decl.staticDeinit.toNull()._and(d => writeLine(() -> dump(d)));
			},
			at(decl is OpaqueAlias) => {
				write("(opaquetype ");
				dumpName(decl, cache);
				
				dump(attrs);

				dump(decl.typevars);

				writeLines(decl.staticMethods, m -> dump(m));
				writeLines(decl.methods, m -> dump(m));
				writeLines(decl.operators, o -> dump(o));
			},
			at(ns is Namespace) => {
				ns.sealed.toNull()._and(sealed => {
					attrs.push(Right(() -> {
						sealed._match(
							at(Some(within)) => {
								write("sealed ");
								dump(within, cache);
							},
							at(None) => {
								write("sealed");
							}
						);
					}));
				});
				ns._match(
					at(decl is Class) => {
						write("(class ");
						dumpName(decl, cache);

						dumpParents(decl.parents, cache);

						if(decl._isStrong) attrs.push(Left("strong"));
						if(decl._isUncounted) attrs.push(Left("uncounted"));
						decl.native.toNull()._and(nat => {
							attrs.push(Right(() -> {
								write("native ");
								dump(nat, cache);
							}));
						});
						dump(attrs);

						dump(decl.typevars);
						
						writeLines(decl.sortedDecls, d -> dump(d));
						writeLines(decl.staticMembers, m -> dump(m));
						writeLines(decl.members, m -> dump(m));
						decl.staticInit.toNull()._and(i => writeLine(() -> dump(i)));
						decl.defaultInit.toNull()._and(i => writeLine(() -> dump(i)));
						writeLines(decl.inits, i -> dump(i));
						writeLines(decl.staticMethods, m -> dump(m));
						writeLines(decl.methods, m -> dump(m));
						writeLines(decl.operators, o -> dump(o));
						decl.staticDeinit.toNull()._and(d => writeLine(() -> dump(d)));
						decl.deinit.toNull()._and(d => writeLine(() -> dump(d)));
						writeLines(decl.categories, c -> dump(c));
					},
					at(decl is Protocol) => {
						write("(protocol ");
						dumpName(decl, cache);

						dumpParents(decl.parents, cache);

						dump(attrs);

						dump(decl.typevars);
						
						writeLines(decl.sortedDecls, d -> dump(d));
						writeLines(decl.staticMembers, m -> dump(m));
						writeLines(decl.members, m -> dump(m));
						decl.staticInit.toNull()._and(i => writeLine(() -> dump(i)));
						decl.defaultInit.toNull()._and(i => writeLine(() -> dump(i)));
						writeLines(decl.inits, i -> dump(i));
						writeLines(decl.staticMethods, m -> dump(m));
						writeLines(decl.methods, m -> dump(m));
						writeLines(decl.operators, o -> dump(o));
						decl.staticDeinit.toNull()._and(d => writeLine(() -> dump(d)));
						decl.deinit.toNull()._and(d => writeLine(() -> dump(d)));
						writeLines(decl.categories, c -> dump(c));
					},
					at(kind is Kind) => {
						if(kind._isFlags) attrs.push(Left("flags"));
						if(kind._isStrong) attrs.push(Left("strong"));
						if(kind._isUncounted) attrs.push(Left("uncounted"));

						kind._match(
							at(decl is ValueKind) => {
								write("(value-kind ");
								dumpName(decl, cache);

								decl.repr.toNull()._and(repr => {
									write(" repr ");
									dump(repr, cache);
								});

								dumpParents(decl.parents, cache);

								dump(attrs);

								dump(decl.typevars);
								
								writeLines(decl.sortedDecls, d -> dump(d));
								writeLines(decl.valueCases, c -> dump(c));
								writeLines(decl.staticMembers, m -> dump(m));
								writeLines(decl.members, m -> dump(m));
								decl.staticInit.toNull()._and(i => writeLine(() -> dump(i)));
								writeLines(decl.staticMethods, m -> dump(m));
								writeLines(decl.methods, m -> dump(m));
								writeLines(decl.operators, o -> dump(o));
								decl.staticDeinit.toNull()._and(d => writeLine(() -> dump(d)));
								decl.deinit.toNull()._and(d => writeLine(() -> dump(d)));
								writeLines(decl.categories, c -> dump(c));
							},
							at(decl is TaggedKind) => {
								write("(tagged-kind ");
								dumpName(decl, cache);

								dumpParents(decl.parents, cache);

								dump(attrs);

								dump(decl.typevars);
								
								writeLines(decl.sortedDecls, d -> dump(d));
								writeLines(decl.taggedCases, c -> dump(c));
								writeLines(decl.staticMembers, m -> dump(m));
								writeLines(decl.members, m -> dump(m));
								decl.staticInit.toNull()._and(i => writeLine(() -> dump(i)));
								decl.defaultInit.toNull()._and(i => writeLine(() -> dump(i)));
								writeLines(decl.staticMethods, m -> dump(m));
								writeLines(decl.methods, m -> dump(m));
								writeLines(decl.operators, o -> dump(o));
								decl.staticDeinit.toNull()._and(d => writeLine(() -> dump(d)));
								decl.deinit.toNull()._and(d => writeLine(() -> dump(d)));
								writeLines(decl.categories, c -> dump(c));
							},
							_ => throw "bad"
						);
					},
					at(decl is Module) => {
						write("(module ");
						dumpName(decl, cache);

						dumpParents(decl.parents, cache);

						if(decl.isMain) attrs.push(Left("main"));
						decl.native.toNull()._and(native => {
							attrs.push(Left('native "${native.name}"'));
						});
						dump(attrs);

						dump(decl.typevars);
						
						writeLines(decl.sortedDecls, d -> dump(d));
						writeLines(decl.staticMembers, m -> dump(m));
						decl.staticInit.toNull()._and(i => writeLine(() -> dump(i)));
						writeLines(decl.staticMethods, m -> dump(m));
						decl.staticDeinit.toNull()._and(d => writeLine(() -> dump(d)));
						writeLines(decl.categories, c -> dump(c));
					},
					_ => throw "bad"
				);
			},
			_ => throw "bad"
		);

		write(")");
	}
	function dumpName(decl: TypeDecl, cache: Cache) {
		write(decl.name.name);
		if(decl.params.length > 0) {
			write(" [");
			write(decl.params, " ", p -> dump(p, cache));
			write("]");
		}
	}


	overload function dump(file: File) {
		write("; " + file.path);
		nextLine();
		nextLine();

		for(imp in file.imported) {
			write("(use ");
			
			if(imp.types.length > 0) {
				imp.types._match(
					at([type]) => {
						dump(type, Nil);
					},
					at(types) => {
						write("[");
						writeLines(types, t -> dump(t, Nil));
						write("]");
					}
				);
				write(" :from ");
			}

			imp.from._match(
				at(decl is AnyTypeDecl) => {
					dump(decl.thisType, Nil);
				},
				at(type is Type) => {
					dump(type, Nil);
				},
				_ => {
					write("???");
				}
			);

			write(")");
			nextLine();
		}

		for(decl in file.sortedDecls.sorted((d1, d2) -> d1.span.start.compare(d2.span.start))) {
			nextLine();

			dump(decl);

			nextLine();
		}

		for(cat in file.categories) {
			nextLine();

			dump(cat);
			
			nextLine();
		}
	}
}