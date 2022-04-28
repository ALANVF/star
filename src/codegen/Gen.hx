package codegen;

import typing.*;
import Util.detuple;

@:publicFields
@:build(util.Overload.build())
class Gen {
	final output: haxe.io.Output;
	var level: UInt;
	final tab: String;
	final tabs: Array<String>;

	function new(out: haxe.io.Output, tabWidth: Int = 4) {
		output = out;
		level = 0;
		tab = " ".repeat(tabWidth);
		tabs = [""];
	}

	@:noCompletion
	inline function _newline() output.writeByte('\n'.code);

	function indent() {
		if(tabs.length - 1 < level) {
			tabs.push(tabs.last() + tab);
		}

		output.writeString(tabs[level]);
	}

	inline function newline() {
		_newline();
		indent();
	}

	overload inline function write(str: String) {
		output.writeString(str);
	}


	function writeDVar(tvar: TypeVar) {
		final id = CodeGen.world.getDVarID(tvar);
		write('type $id ');
		write(tvar.name.name);
		// NO HKTS YET PLS
		// ...
	}
	overload function writeDVars(tvars: MultiMap<String, TypeVar>) {
		if(tvars.size > 0) {
			write("given ");
			writeBlock(tvars.allValues(), tvar -> {
				writeDVar(tvar);
			});
			newline();
		}
	}

	overload function write(native: NativeKind) native._match(
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
			write(CodeGen.world.getTypeRef(t));
		}
	);

	overload function write(cls: Class) {
		writeDVars(cls.typevars);
		
		final id = CodeGen.world.getID(cls);
		write('class $id ${Type.getFullPath(cls).value()}');

		if(cls.params.length > 0) {
			write("[_");
			for(_ in 0...(cls.params.length - 1)) write(", _");
			write("] ");
		} else {
			write(" ");
		}
		
		if(cls.parents.length > 0) {
			writeBlock(cls.parents, "of #[", "] ", p -> write(CodeGen.world.getTypeRef(p)));
		}

		cls.refinees._match(
			at([]) => {},
			at([ref]) => {
				write('is refinement ${CodeGen.world.getID(ref)} ');
			},
			at(refinees) => {
				writeBlock(refinees, "is refinement #[", "] ", r -> write('${CodeGen.world.getID(r)}'));
			}
		);

		if(cls.friends.length > 0) {
			writeBlock(cls.friends, "is friend #[", "] ", f -> write(CodeGen.world.getTypeRef(f)));
		}

		cls.hidden._and(hidden => hidden._match(
			at(None) => write("is hidden "),
			at(Some(within)) => {
				write("is hidden ");
				write(CodeGen.world.getTypeRef(within));
				write(" ");
			}
		));

		cls.sealed.forEach(sealed -> sealed._match(
			at(None) => write("is sealed "),
			at(Some(within)) => {
				write("is sealed ");
				write(CodeGen.world.getTypeRef(within));
				write(" ");
			}
		));

		if(cls._isStrong) write("is strong ");
		if(cls._isUncounted) write("is uncounted ");

		cls.native.forEach(nat -> {
			write("is native ");
			write(nat);
			write(" ");
		});

		level++;
		write("{");

		if(cls.sortedDecls.length > 0 || cls.categories.length > 0) throw "TODO";
		
		final staticMemberInits: Array<Tuple2<Member, TExpr>> = [];
		for(mem in cls.staticMembers) {
			newline();
			write(mem, true, staticMemberInits);
			newline();
		}

		final instMemberInits: Array<Tuple2<Member, TExpr>> = [];
		for(mem in cls.members) {
			newline();
			write(mem, false, instMemberInits);
			newline();
		}

		cls.staticInit.doOrElse(init => {
			newline();
			write(init, staticMemberInits);
			newline();
		}, {
			if(staticMemberInits.length > 0) {
				// TODO: lazy
				newline();
				final init: StaticInit = untyped $new(typing.StaticInit);
				init.typedBody = [];
				write(init, staticMemberInits);
				newline();
			}
		});

		cls.defaultInit.doOrElse(init => {
			newline();
			write(init, instMemberInits);
			newline();
		}, {
			if(instMemberInits.length > 0) {
				// TODO: lazy
				newline();
				final init: DefaultInit = untyped $new(typing.DefaultInit);
				init.typedBody = [];
				write(init, instMemberInits);
				newline();
			}
		});

		for(init in cls.inits) {
			newline();
			write(init);
			newline();
		}

		for(mth in cls.staticMethods) {
			newline();
			write(mth);
			newline();
		}

		for(mth in cls.methods) {
			newline();
			write(mth);
			newline();
		}

		for(oper in cls.operators) {
			newline();
			write(oper);
			newline();
		}

		cls.deinit.forEach(deinit -> {
			newline();
			write(deinit, null);
			newline();
		});

		cls.staticDeinit.forEach(deinit -> {
			newline();
			write(deinit, null);
			newline();
		});

		level--;
		newline();
		write("}");
	}

	overload function write(proto: Protocol) {
		writeDVars(proto.typevars);
		
		final id = CodeGen.world.getID(proto);
		write('protocol $id ${Type.getFullPath(proto).value()}');

		if(proto.params.length > 0) {
			write("[_");
			for(_ in 0...(proto.params.length - 1)) write(", _");
			write("] ");
		} else {
			write(" ");
		}
		
		if(proto.parents.length > 0) {
			writeBlock(proto.parents, "of #[", "] ", p -> write(CodeGen.world.getTypeRef(p)));
		}

		proto.refinees._match(
			at([]) => {},
			at([ref]) => {
				write('is refinement ${CodeGen.world.getID(ref)} ');
			},
			at(refinees) => {
				writeBlock(refinees, "is refinement #[", "] ", r -> write('${CodeGen.world.getID(r)}'));
			}
		);

		if(proto.friends.length > 0) {
			writeBlock(proto.friends, "is friend #[", "] ", f -> write(CodeGen.world.getTypeRef(f)));
		}

		proto.hidden._and(hidden => hidden._match(
			at(None) => write("is hidden "),
			at(Some(within)) => {
				write("is hidden ");
				write(CodeGen.world.getTypeRef(within));
				write(" ");
			}
		));

		proto.sealed.forEach(sealed -> sealed._match(
			at(None) => write("is sealed "),
			at(Some(within)) => {
				write("is sealed ");
				write(CodeGen.world.getTypeRef(within));
				write(" ");
			}
		));

		level++;
		write("{");

		if(proto.sortedDecls.length > 0 || proto.categories.length > 0) throw "TODO";
		
		final staticMemberInits: Array<Tuple2<Member, TExpr>> = [];
		for(mem in proto.staticMembers) {
			newline();
			write(mem, true, staticMemberInits);
			newline();
		}

		final instMemberInits: Array<Tuple2<Member, TExpr>> = [];
		for(mem in proto.members) {
			newline();
			write(mem, false, instMemberInits);
			newline();
		}

		proto.staticInit.doOrElse(init => {
			newline();
			write(init, staticMemberInits);
			newline();
		}, {
			if(staticMemberInits.length > 0) {
				// TODO: lazy
				newline();
				final init: StaticInit = untyped $new(typing.StaticInit);
				init.typedBody = [];
				write(init, staticMemberInits);
				newline();
			}
		});

		proto.defaultInit.doOrElse(init => {
			newline();
			write(init, instMemberInits);
			newline();
		}, {
			if(instMemberInits.length > 0) {
				// TODO: lazy
				newline();
				final init: DefaultInit = untyped $new(typing.DefaultInit);
				init.typedBody = [];
				write(init, instMemberInits);
				newline();
			}
		});

		for(init in proto.inits) {
			newline();
			write(init);
			newline();
		}

		for(mth in proto.staticMethods) {
			newline();
			write(mth);
			newline();
		}

		for(mth in proto.methods) {
			newline();
			write(mth);
			newline();
		}

		for(oper in proto.operators) {
			newline();
			write(oper);
			newline();
		}

		proto.deinit.forEach(deinit -> {
			newline();
			write(deinit, null);
			newline();
		});

		proto.staticDeinit.forEach(deinit -> {
			newline();
			write(deinit, null);
			newline();
		});

		level--;
		newline();
		write("}");
	}

	overload function write(kind: TaggedKind) {
		writeDVars(kind.typevars);
		
		final id = CodeGen.world.getID(kind);
		write('tagged-kind $id ${Type.getFullPath(kind).value()}');

		if(kind.params.length > 0) {
			write("[_");
			for(_ in 0...(kind.params.length - 1)) write(", _");
			write("] ");
		} else {
			write(" ");
		}
		
		if(kind.parents.length > 0) {
			writeBlock(kind.parents, "of #[", "] ", p -> write(CodeGen.world.getTypeRef(p)));
		}

		kind.refinees._match(
			at([]) => {},
			at([ref]) => {
				write('is refinement ${CodeGen.world.getID(ref)} ');
			},
			at(refinees) => {
				writeBlock(refinees, "is refinement #[", "] ", r -> write('${CodeGen.world.getID(r)}'));
			}
		);

		if(kind.friends.length > 0) {
			writeBlock(kind.friends, "is friend #[", "] ", f -> write(CodeGen.world.getTypeRef(f)));
		}

		kind.hidden._and(hidden => hidden._match(
			at(None) => write("is hidden "),
			at(Some(within)) => {
				write("is hidden ");
				write(CodeGen.world.getTypeRef(within));
				write(" ");
			}
		));

		kind.sealed.forEach(sealed -> sealed._match(
			at(None) => write("is sealed "),
			at(Some(within)) => {
				write("is sealed ");
				write(CodeGen.world.getTypeRef(within));
				write(" ");
			}
		));

		if(kind._isStrong) write("is strong ");
		if(kind._isUncounted) write("is uncounted ");
		if(kind._isFlags) write("is flags ");

		level++;
		write("{");

		if(kind.sortedDecls.length > 0 || kind.categories.length > 0) throw "TODO";

		for(tcase in kind.taggedCases) {
			newline();
			write(tcase);
			newline();
		}
		
		final staticMemberInits: Array<Tuple2<Member, TExpr>> = [];
		for(mem in kind.staticMembers) {
			newline();
			write(mem, true, staticMemberInits);
			newline();
		}

		final instMemberInits: Array<Tuple2<Member, TExpr>> = [];
		for(mem in kind.members) {
			newline();
			write(mem, false, instMemberInits);
			newline();
		}

		kind.staticInit.doOrElse(init => {
			newline();
			write(init, staticMemberInits);
			newline();
		}, {
			if(staticMemberInits.length > 0) {
				// TODO: lazy
				newline();
				final init: StaticInit = untyped $new(typing.StaticInit);
				init.typedBody = [];
				write(init, staticMemberInits);
				newline();
			}
		});

		kind.defaultInit.doOrElse(init => {
			newline();
			write(init, instMemberInits);
			newline();
		}, {
			if(instMemberInits.length > 0) {
				// TODO: lazy
				newline();
				final init: DefaultInit = untyped $new(typing.DefaultInit);
				init.typedBody = [];
				write(init, instMemberInits);
				newline();
			}
		});

		for(mth in kind.staticMethods) {
			newline();
			write(mth);
			newline();
		}

		for(mth in kind.methods) {
			newline();
			write(mth);
			newline();
		}

		for(oper in kind.operators) {
			newline();
			write(oper);
			newline();
		}

		kind.deinit.forEach(deinit -> {
			newline();
			write(deinit, null);
			newline();
		});

		kind.staticDeinit.forEach(deinit -> {
			newline();
			write(deinit, null);
			newline();
		});

		level--;
		newline();
		write("}");
	}


	overload function write(mem: Member, isStatic: Bool, inits: Array<Tuple2<Member, TExpr>>) {
		final id = isStatic? CodeGen.world.getStaticID(mem) : CodeGen.world.getInstID(mem);

		write('my $id ${mem.name.name} (');
		write(CodeGen.world.getTypeRef(mem.type._or(throw "Member does not have a type: "+mem.name.name)));
		write(")");

		if(isStatic) write(" is static");
		
		mem.hidden._and(hidden => hidden._match(
			at(None) => write(" is hidden"),
			at(Some(within)) => {
				write(" is hidden ");
				write(CodeGen.world.getTypeRef(within));
			}
		));

		if(mem.isReadonly) write(" is readonly");

		// TODO
		mem.getter._and(getter => getter._match(
			at(None) => write(' is getter `${mem.name.name}`'),
			at(Some({name: name})) => write(' is getter `${name}`')
		));
		mem.setter._and(setter => setter._match(
			at(None) => write(' is setter `${mem.name.name}`'),
			at(Some({name: name})) => write(' is setter `${name}`')
		));

		if(mem.noInherit) write(" is noinherit");

		mem.typedValue._and(tvalue => {
			inits.push(tuple(mem, tvalue));
		});
	}

	overload function write(tcase: TaggedCase) {
		final id = CodeGen.world.getID(tcase);

		// TODO: assoc
		write('has $id [');
		tcase._match(
			at(sc is SingleTaggedCase) => {
				write(sc.name.name);
			},
			at(mc is MultiTaggedCase) => {
				level++;
				for(p in mc.params) {
					newline();
					write(p.label.name+": "+p.name.name+" (");
					write(CodeGen.world.getTypeRef(p.type));
					write(")");
					// ...
				}
				level--;
				newline();
			},
			_ => throw "bad"
		);
		write("]");

		tcase.typedInit._and(init => {
			write(" ");
			writeBlock(CodeGen.compile(new GenCtx(), init));
		});
	}


	function writeMVar(tvar: TypeVar) {
		final id = CodeGen.world.getMVarID(tvar);
		write('type $id ');
		write(tvar.name.name);
		// NO HKTS YET PLS
		// ...
	}
	overload function writeMVars(tvars: MultiMap<String, TypeVar>) {
		if(tvars.size > 0) {
			write("given ");
			writeBlock(tvars.allValues(), tvar -> {
				writeMVar(tvar);
			});
			newline();
		}
	}

	overload function write(init: Init) {
		if(init.isMacro) return;

		init._match(
			at({typevars: tvars} is MultiInit) => writeMVars(tvars),
			_ => {}
		);

		var paramNames: Null<Array<String>> = null;
		write('init ${CodeGen.world.getID(init)} [');
		init._match(
			at(si is SingleInit) => {
				write(si.name.name);
			},
			at(mi is MultiInit) => {
				final _paramNames = [];
				
				level++;
				for(p in mi.params) {
					_paramNames.push(p.name.name);

					newline();
					write(p.label.name+": "+p.name.name+" (");
					write(CodeGen.world.getTypeRef(p.type));
					write(")");
					// ...
				}
				level--;
				newline();

				paramNames = _paramNames;
			},
			_ => throw "bad"
		);
		write("] ");

		init.hidden._and(hidden => hidden._match(
			at(None) => write("is hidden "),
			at(Some(within)) => {
				write("is hidden ");
				write(CodeGen.world.getTypeRef(within));
				write(" ");
			}
		));
		if(init.noInherit) write("is noinherit ");

		init.typedBody._andOr(tb => {
			writeBlock(CodeGen.compile(new GenCtx(), tb));
		}, {
			init.native._andOr(nat => nat._match(
				at(None) => throw "bad",
				at(Some({name: native})) => {
					final ops: Opcodes = [];
					paramNames._and(names => {
						for(name in names) {
							ops.push(OGetLocal(name));
						}
					});
					ops.push(ONative(native));
					ops.push(ORet);
					writeBlock(ops);
				}
			), {
				write("is virtual "); // TODO: elaborate on this?
			});
		});
	}
	
	overload function write(mth: StaticMethod) {
		if(mth.isMacro) return;

		mth._match(
			at({typevars: tvars} is MultiStaticMethod) => writeMVars(tvars),
			_ => {}
		);

		var paramNames: Null<Array<String>> = null;
		write('on ${CodeGen.world.getID(mth)} [');
		mth._match(
			at(sm is SingleStaticMethod) => {
				write(sm.name.name);
			},
			at(mm is MultiStaticMethod) => {
				final _paramNames = [];
				
				level++;
				for(p in mm.params) {
					_paramNames.push(p.name.name);

					newline();
					write(p.label.name+": "+p.name.name+" (");
					write(CodeGen.world.getTypeRef(p.type));
					write(")");
					// ...
				}
				level--;
				newline();

				paramNames = _paramNames;
			},
			_ => throw "bad"
		);
		write("] (");
		write(CodeGen.world.getTypeRef(mth.ret._or(Pass2.STD_Void.thisType)));
		write(") ");

		write("is static ");
		mth.hidden._and(hidden => hidden._match(
			at(None) => write("is hidden "),
			at(Some(within)) => {
				write("is hidden ");
				write(CodeGen.world.getTypeRef(within));
				write(" ");
			}
		));
		if(mth.noInherit) write("is noinherit ");

		mth.typedBody._andOr(tb => {
			writeBlock(CodeGen.compile(new GenCtx(), tb));
		}, {
			mth.native._andOr(nat => nat._match(
				at(None) => throw "bad",
				at(Some({name: native})) => {
					final ops: Opcodes = [];
					paramNames._and(names => {
						for(name in names) {
							ops.push(OGetLocal(name));
						}
					});
					ops.push(ONative(native));
					if(mth.ret._andOr(ret => !ret.isNative(NVoid), false)) {
						ops.push(ORet);
					} else {
						ops.push(ORetVoid);
					}
					writeBlock(ops);
				}
			), {
				write("is virtual "); // TODO: elaborate on this?
			});
		});
	}
	
	overload function write(mth: Method) {
		if(mth.isMacro) return;

		mth._match(
			at({typevars: tvars} is MultiMethod | {typevars: tvars} is CastMethod) => writeMVars(tvars),
			_ => {}
		);

		var paramNames: Null<Array<String>> = null;
		write('on ${CodeGen.world.getID(mth)} [');
		mth._match(
			at(sm is SingleMethod) => {
				write(sm.name.name);
			},
			at(mm is MultiMethod) => {
				final _paramNames = [];
				
				level++;
				for(p in mm.params) {
					_paramNames.push(p.name.name);

					newline();
					write(p.label.name+": "+p.name.name+" (");
					write(CodeGen.world.getTypeRef(p.type));
					write(")");
					// ...
				}
				level--;
				newline();

				paramNames = _paramNames;
			},
			at(cm is CastMethod) => {
				cm.ret = cm.type; // hacky thing, ideally just use ret for everything?
				write(CodeGen.world.getTypeRef(cm.type));
			},
			_ => throw "bad"
		);
		write("] (");
		write(CodeGen.world.getTypeRef(mth.ret._or(Pass2.STD_Void.thisType)));
		write(") ");

		if(mth.isMain) write("is main ");
		mth.hidden._and(hidden => hidden._match(
			at(None) => write("is hidden "),
			at(Some(within)) => {
				write("is hidden ");
				write(CodeGen.world.getTypeRef(within));
				write(" ");
			}
		));
		if(mth.noInherit) write("is noinherit ");

		mth.typedBody._andOr(tb => {
			writeBlock(CodeGen.compile(new GenCtx(), tb));
		}, {
			mth.native._andOr(nat => nat._match(
				at(None) => throw "bad",
				at(Some({name: native})) => {
					final ops: Opcodes = [OThis];
					paramNames._and(names => {
						for(name in names) {
							ops.push(OGetLocal(name));
						}
					});
					ops.push(ONative(native));
					if(mth.ret._andOr(ret => !ret.isNative(NVoid), false)) {
						ops.push(ORet);
					} else {
						ops.push(ORetVoid);
					}
					writeBlock(ops);
				}
			), {
				write("is virtual "); // TODO: elaborate on this?
			});
		});
	}

	overload function write(oper: Operator) {
		if(oper.isMacro) return;
		
		oper._match(
			at({typevars: tvars} is BinaryOperator) => writeMVars(tvars),
			_ => {}
		);

		var paramName: Null<String> = null;
		write('operator ${CodeGen.world.getID(oper)} `${oper.opName()}` ');
		oper._match(
			at(uo is UnaryOperator) => {
				// empty
			},
			at(bo is BinaryOperator) => {
				write("[");
				level++;
				
				paramName = bo.paramName.name;

				newline();
				write(bo.paramName.name+" (");
				write(CodeGen.world.getTypeRef(bo.paramType));
				write(")");
				// ...

				level--;
				newline();
				write("] ");
			},
			_ => throw "bad"
		);
		write("(");
		write(CodeGen.world.getTypeRef(oper.ret._or(Pass2.STD_Void.thisType)));
		write(") ");

		oper.hidden._and(hidden => hidden._match(
			at(None) => write("is hidden "),
			at(Some(within)) => {
				write("is hidden ");
				write(CodeGen.world.getTypeRef(within));
				write(" ");
			}
		));
		if(oper.noInherit) write("is noinherit ");

		oper.typedBody._andOr(tb => {
			writeBlock(CodeGen.compile(new GenCtx(), tb));
		}, {
			oper.native._andOr(nat => nat._match(
				at(None) => throw "bad",
				at(Some({name: native})) => {
					final ops: Opcodes = [OThis];
					paramName._and(name => {
						ops.push(OGetLocal(name));
					});
					ops.push(ONative(native));
					ops.push(ORet);
					writeBlock(ops);
				}
			), {
				write("is virtual "); // TODO: elaborate on this?
			});
		});
	}

	overload function write(mth: EmptyMethod, memberInits: Null<Array<Tuple2<Member, TExpr>>>) {
		var isStaticInit = false;
		var isDefaultInit = false;
		mth._match(
			at(_ is DefaultInit) => { isDefaultInit = true; write("default-init "); },
			at(_ is StaticInit) => { isStaticInit = true; write("static-init "); },
			at(_ is Deinit) => write("deinit "),
			at(_ is StaticDeinit) => write("static-deinit "),
			_ => throw "bad"
		);

		mth.typedBody._andOr(tb => {
			var ops: Opcodes = [];
			final ctx = new GenCtx();

			if(isStaticInit) {
				for(mi in memberInits.nonNull()) { detuple(@final [mem, expr] = mi);
					ops = ops.concat(CodeGen.compile(ctx, expr));
					ops.push(OSetStaticField(mem.name.name));
				}
			} else if(isDefaultInit) {
				for(mi in memberInits.nonNull()) { detuple(@final [mem, expr] = mi);
					ops = ops.concat(CodeGen.compile(ctx, expr));
					ops.push(OSetField(mem.name.name));
				}
			}

			ops = ops.concat(CodeGen.compile(ctx, tb));
			
			writeBlock(ops);
		}, {
			throw "bad";
		});
	}

	
	overload function writeBlock(opcodes: Opcodes) {
		write("{");
		level++;
			write(opcodes);
		level--;
		newline();
		write("}");
	}
	overload inline function writeBlock<T>(values: Iterable<T>, fn: (value: T) -> Void) {
		write("{");
		level++;
			for(value in values) {
				newline();
				fn(value);
			}
		level--;
		newline();
		write("}");
	}
	overload inline function writeBlock<T>(values: Iterable<T>, begin: String, end: String, fn: (value: T) -> Void) {
		write(begin);
		level++;
			for(value in values) {
				newline();
				fn(value);
			}
		level--;
		newline();
		write(end);
	}
	overload inline function writeBlock<K, V>(values: KeyValueIterable<K, V>, fn: (key: K, value: V) -> Void) {
		write("{");
		level++;
			for(key => value in values) {
				newline();
				fn(key, value);
			}
		level--;
		newline();
		write("}");
	}

	overload function write(opcodes: Opcodes) {
		for(opcode in opcodes) {
			newline();
			write(opcode);
		}
	}

	overload function write(opcode: Opcode) opcode._match(
		at(ONewLocal(name, t)) => {
			write('new-local $name ');
			write(t);
		},
		at(OGetLocal(name)) => write('get-local $name'),
		at(OSetLocal(name)) => write('set-local $name'),
		at(OTeeLocal(name)) => write('tee-local $name'),

		at(OGetField(name)) => write('get-field $name'),
		at(OSetField(name)) => write('set-field $name'),
		at(OTeeField(name)) => write('tee-field $name'),

		at(OGetStaticField(name)) => write('get-static-field $name'),
		at(OSetStaticField(name)) => write('set-static-field $name'),
		at(OTeeStaticField(name)) => write('tee-static-field $name'),

		at(ODup) => write("dup"),
		at(ODup2) => write("dup2"),
		at(OSwap) => write("swap"),
		at(OPop) => write("pop"),

		at(OIf(then)) => {
			write("if ");
			writeBlock(then);
		},
		at(OIfNot(then)) => {
			write("ifnot ");
			writeBlock(then);
		},
		at(OIfElse(then, _else)) => {
			write("if ");
			writeBlock(then);
			write(" else ");
			writeBlock(_else);
		},

		at(ODo(label, _do)) => {
			write('do "$label" ');
			writeBlock(_do);
		},
		
		at(OLoop(label, loop)) => {
			write('loop "$label" ');
			writeBlock(loop);
		},
		at(OLoopThen(label, loop, then)) => {
			write('loop "$label" ');
			writeBlock(loop);
			write(" then ");
			writeBlock(then);
		},

		at(OTry(_try, _catch)) => {
			write("try ");
			writeBlock(_try);
			write(" catch ");
			writeBlock(_catch);
		},

		at(ORet) => write("ret"),
		at(ORetVoid) => write("retvoid"),

		at(OThrow(info)) => write('throw "$info"'),
		at(ORethrow) => write("rethrow"),

		//at(OBreak) => write("break"),
		//at(OBreakDepth(depth)) => write('break-depth $depth'),
		at(OBreak(label)) => write('break "$label"'),

		//at(ONext) => write("next"),
		//at(ONextDepth(depth)) => write('next-depth $depth'),
		at(ONext(label)) => write('next "$label"'),

		at(ONative(native)) => write('native $native'),

		at(OInt8(int, true)) => write('int8 $int'),
		at(OInt8(int, false)) => write('uint8 $int'),
		at(OInt16(int, true)) => write('int16 $int'),
		at(OInt16(int, false)) => write('uint16 $int'),
		at(OInt32(int, true)) => write('int32 $int'),
		at(OInt32(int, false)) => write('uint32 $int'),
		at(OInt64(int, true)) => write('int64 $int'),
		at(OInt64(int, false)) => write('uint64 $int'),

		at(ODec32(int, dec, null)) => write('dec32 $int.$dec'),
		at(ODec32(int, dec, exp!!)) => write('dec32 $int.${dec}e$exp'),
		at(ODec64(int, dec, null)) => write('dec64 $int.$dec'),
		at(ODec64(int, dec, exp!!)) => write('dec64 $int.${dec}e$exp'),

		//at(OChar(char)) => write('char ${char.toInt()}'),

		at(OStr(str)) => write('str "${str.escape()}"'),

		at(OTrue) => write("true"),
		at(OFalse) => write("false"),

		at(OThis) => write("this"),

		at(OBlock(body)) => {
			write("block ");
			writeBlock(body);
		},

		at(OTCaseID(t, tag)) => {
			write("tcase-id ");
			write(t);
			write(' $tag');
		},
		at(OVCaseID(t, tag)) => {
			write("vcase-id ");
			write(t);
			write(' $tag');
		},
		at(OKindID) => write("kind-id"),
		at(OKindSlot(i)) => write('kind-slot $i'),

		at(OUpcast(t)) => {
			write("upcast ");
			write(t);
		},
		at(ODowncast(t)) => {
			write("downcast ");
			write(t);
		},
		at(ONativeCast(t)) => {
			write("nativecast ");
			write(t);
		},

		at(OOfType(t)) => {
			write("of-type ");
			write(t);
		},

		at(OGetMember(id)) => write('get-member $id'),
		at(OSetMember(id)) => write('set-member $id'),
		at(OTeeMember(id)) => write('tee-member $id'),

		at(OGetStaticMember(t, id)) => {
			write('get-static-member ');
			write(t);
			write(' $id');
		},
		at(OSetStaticMember(t, id)) => {
			write('set-static-member ');
			write(t);
			write(' $id');
		},
		at(OTeeStaticMember(t, id)) => {
			write('tee-static-member ');
			write(t);
			write(' $id');
		},

		at(ODefaultInit(t)) => {
			write("default-init ");
			write(t);
		},

		at(OInitThis_S(_super, id)) => {
			write("init-this-s ");
			write(_super);
			write(' $id');
		},
		at(OInitThis_M(_super, id, ctx)) => {
			write("init-this-m ");
			write(_super);
			write(' $id');
			ctx._and(ctx => write(ctx));
		},

		at(OSend_IS(t, id)) => {
			write("sendinit-s ");
			write(t);
			write(' $id');
		},

		at(OSend_IM(t, id, ctx)) => {
			write("sendinit-m ");
			write(t);
			write(' $id');
			ctx._and(ctx => write(ctx));
		},

		at(OSend_SS(t, id)) => {
			write("send-ss ");
			write(t);
			write(' $id');
		},

		at(OSend_MS(t, id, ctx)) => {
			write("send-ms ");
			write(t);
			write(' $id');
			ctx._and(ctx => write(ctx));
		},
		
		at(OSend_SI(t, id)) => {
			write("send-si ");
			write(t);
			write(' $id');
		},
		at(OSendDynamic_SI(t, id)) => {
			write("send-dyn-si ");
			write(t);
			write(' $id');
		},

		at(OSend_MI(t, id, ctx)) => {
			write("send-mi ");
			write(t);
			write(' $id');
			ctx._and(ctx => write(ctx));
		},
		at(OSendDynamic_MI(t, id, ctx)) => {
			write("send-dyn-mi ");
			write(t);
			write(' $id');
			ctx._and(ctx => write(ctx));
		},

		at(OSend_C(t, id, ctx)) => {
			write("send-c ");
			write(t);
			write(' $id');
			ctx._and(ctx => write(ctx));
		},
		at(OSendDynamic_C(t, id, ctx)) => {
			write("send-dyn-c ");
			write(t);
			write(' $id');
			ctx._and(ctx => write(ctx));
		},

		at(OSend_BO(t, id, ctx)) => {
			write("send-bo ");
			write(t);
			write(' $id');
			ctx._and(ctx => write(ctx));
		},
		at(OSendDynamic_BO(t, id, ctx)) => {
			write("send-dyn-bo ");
			write(t);
			write(' $id');
			ctx._and(ctx => write(ctx));
		},

		at(OSend_UO(t, id)) => {
			write("send-uo ");
			write(t);
			write(' $id');
		},
		at(OSendDynamic_UO(t, id)) => {
			write("send-dyn-uo ");
			write(t);
			write(' $id');
		},

		at(OInitClass(t)) => {
			write("init-class ");
			write(t);
		},
		at(OInitTKind(t, tag)) => {
			write("init-tkind ");
			write(t);
			write(' $tag');
		},
		at(OInitVKind(t, tag)) => {
			write("init-vkind ");
			write(t);
			write(' $tag');
		},
		at(OInitMultiTKind(t, tag)) => {
			write("init-multi-tkind ");
			write(t);
			write(' $tag');
		},
		at(OInitMultiVKind(t, tag)) => {
			write("init-multi-tkind ");
			write(t);
			write(' $tag');
		},

		at(OMultiKindHasTag(tag)) => {
			write('multi-kind-has-tag $tag');
		},
		at(OMultiKindGetTag(tag)) => {
			write('multi-kind-get-tag $tag');
		}
	);

	overload function write(type: TypeRef) {
		type._match(
			at(TDecl(id)) => write('decl#$id'),
			at(TInst(id, inst)) => {
				write('inst#$id');
				writeDVars(inst);
			},
			at(TTypeVar(id)) => write(id),
			at(TThis) => write('this')
		);
	}

	overload function writeDVars(inst: TypeInstCtx) {
		writeBlock(inst, (id, t) -> {
			write('dvar#$id => ');
			write(t);
		});
	}

	overload function writeMVars(inst: TypeInstCtx) {
		writeBlock(inst, (id, t) -> {
			write('mvar#$id => ');
			write(t);
		});
	}

	overload function write(inst: TVarInstCtx) {
		writeBlock(inst, (tv, t) -> {
			write(tv);
			write(" => ");
			write(t);
		});
	}

	overload function write(tvar: TVar) {
		tvar._match(
			at(VDecl(id)) => write('dvar#$id'),
			at(VMethod(id)) => write('mvar#$id')
		);
	}
}

/*
class Cursor {
public:
	Star::Core::Int ISLOT$line;
	Star::Core::Int ISLOT$column;
	Star::Core::Bool ISLOT$lastWasCR;

	Star::Core::Int INST$line() { return ISLOT$line; }
	Star::Core::Int INST$column() { return ISLOT$column; }

	Cursor() {}

	static Cursor* INIT$new() {
		auto $this = new Cursor();
		$this->ISLOT$line = Star::Core::Int(0);
		$this->ISLOT$column = Star::Core::Int(0);
		$this->ISLOT$lastWasCr = Star::Core::Bool(false);
		return $this;
	}

	Cursor* INST$new() {
		auto $result = new Cursor();
		$result->ISLOT$line = ISLOT$line;
		$result->ISLOT$column = ISLOT$column;
		$result->ISLOT$lastWasCr = ISLOT$lastWasCr;
		return $result;
	}

	Star::Core::Bool operator ==(Cursor* other) {
		return Star::Core::Bool(
			ISLOT$line == other->ISLOT$line &&
			ISLOT$column == other->ISLOT$column &&
			ISLOT$lastWasCR == other->ISLOT$lastWasCR
		);
	}

	inline Pos* INST$pos() {
		auto $0 = new Pos();
		$0->ISLOT$line = ISLOT$line;
		$0->ISLOT$column = ISLOT$column;
		return $0;
	}

	void INST$append$(Star::Core::Str* str) {
		{
			auto i = Star::Core::Int(0);
			auto upto$0 = str->INST$length();
			for(; i < upto$0; i++) {
				this->INST$append$($CAT<Star::Core::Unsafe>(str)->INST$at$(str, i));
			}
		}
	}

	void INST$append$(Star::Core::Char $char) {
		switch((char) $char) {
			case '\r': {
				ISLOT$line++;
				ISLOT$column = Star::Core::Int(0);
				if(!(bool) ISLOT$lastWasCR) {
					ISLOT$lastWasCR = Star::Core::Bool(true);
				}
				break;
			}
			case '\n': {
				if(!ISLOT$lastWasCR) {
					ISLOT$line++;
					ISLOT$column = Star::Core::Int(0);
				} else {
					ISLOT$lastWasCR = Star::Core::Bool(false);
				}
				break;
			}
			default: {
				if((Star::Core::Int(31) < $char && $char < Star::Core::Int(127)) || $char == Star::Core::Char('\t')) {
					ISLOT$column++;
				}

				if(ISLOT$lastWasCR) {
					ISLOT$lastWasCR = Star::Core::Bool(false);
				}
			}
		}
	}
};
*/

/*
class Cursor {
	has Star::Core::Int $.SLOT-line is rw = Star::Core::Int.new(0);
	has Star::Core::Int $.SLOT-column is rw = Star::Core::Int.new(0);
	has Star::Core::Bool $.SLOT-lastWasCR is rw = Star::Core::Bool.new(false);

	method 'line'(::?CLASS:D: --> Star::Core::Int) { $!SLOT-line }
	method 'column'(::?CLASS:D: --> Star::Core::Int) { $!SLOT-column }

	method 'pos'(::?CLASS:D: --> Pos) {
		my Pos $_0 = Pos.new();
		$_0.'line:'($!SLOT-line);
		$_0.'column:'($!SLOT-column);
		return $_0;
	}

	multi method 'append:'(::?CLASS:D: Star::Core::Str $str --> Nil) {
		{
			my Star::Core::Int $i = Star::Core::Int.new(0);
			my Star::Core::Int $_0upto = $str.'length'();
			loop (; $i.'<'($_0upto); $i = $i.'++'()) {
				self.'append:'($str.'at:'(Category[Star::Core::Unsafe], $i));
			}
		}
	}

	multi method 'append:'(::?CLASS:D: Star::Core::Char $char --> Nil) {
		given $char.SLOT-value {
			when "\r".ord {
				$!SLOT-line = $!SLOT-line.'++'();
				$!SLOT-column = Star::Core::Int.new(0);
				$!SLOT-lastWasCR ||= Star::Core::Bool.new(true);
			}
			when "\n".ord {
				if $!SLOT-lastWasCR.'!'() {
					$!SLOT-line = $!SLOT-line.'++'();
					$!SLOT-column = Star::Core::Int.new(0);
				} else {
					$!SLOT-lastWasCR ||= Star::Core::Bool.new(false);
				}
			}
			default {
				if (
					Star::Core::Int.new(31).'<'($char) && $char.'<'(Star::Core::Int.new(127))
					||
					$char.'?='(Star::Core::Char.new("\t".ord))
				) {
					$!SLOT-column = $!SLOT-column.'++'();
				}

				$!SLOT-lastWasCR &&= Star::Core::Bool.new(false);
			}
		}
	}
}
*/

/*
(class Cursor
    (my line (type #Star.Core.Int) is [getter] (int 0))
    (my column (type #Star.Core.Int) is [getter] (int 0))
    (my lastWasCR (type #Star.Core.Bool) is [hidden] (bool false))
    (on [pos] (type #Pos) is [inline]
        (return (typed
            (type #Pos)
            (send
                (type #Pos)
                :multi { :memberwise-init [#line: #column:] }
                line: (typed
                    (type #Star.Core.Int)
                    (get-slot line))
                column: (typed
                    (type #Star.Core.Int)
                    (get-slot column))))))
    (on [
        append: str (type #Star.Core.Str)
    ]
        (for (my i) :from (int 0) :upto (typed
            (type #Star.Core.Int)
            (send
                (typed
                    (type #Star.Core.Str)
                    (get str))
                :get-member #length))
            (send
                (typed
                    (type :this #Cursor)
                    (this))
                :multi { :multi-method #append: :from (type #Cursor) }
                append: (typed
                    (type :T (category (type #Star.Core.Unsafe) for (type #Star.Core.Values [(type :T (category ...))] {
                        :T (type :T (category ...))
                    })))
                    (send
                        (typed
                            (type #Star.Core.Str)
                            (get str))
                        :multi { :category (type #Star.Core.Unsafe)
                            :multi-method #at: :from (type #Star.Core.Values [(type :T (category (type #Star.Core.Unsafe) for (type #Star.Core.Values [(type :T (category ...))] {
                                :T (type :T (category ...))
                            })))] {
                                :T (type :T (category (type #Star.Core.Unsafe) for (type #Star.Core.Values [(type :T (category ...))] {
                                    :T (type :T (category ...))
                                })))
                            }) }
                        at: (typed
                            (type #Star.Core.Int)
                            (get i)))))))
    (on [
        append: char (type #Star.Core.Char)
    ]
        (match (typed
            (type #Star.Core.Char)
            (get char))
            (at (char #"\r")
                (send
                    (typed
                        (type #Star.Core.Int)
                        (get-slot line))
                    :suffix
                    :unary #++)
                (set-slot column
                    (int 0))
                (set-slot lastWasCR
                    (typed
                        (type #Star.Core.Bool)
                        (send
                            (typed
                                (type #Star.Core.Bool)
                                (get-slot lastWasCR))
                            :infix { :binary #|| :from (type #Star.Core.Bool) }
                            (bool true)))))
            (at (char #"\n")
                (if (typed
                    (type #Star.Core.Bool)
                    (send
                        (typed
                            (type #Star.Core.Bool)
                            (get-slot lastWasCR))
                        :prefix
                        :unary #!))
                    (send
                        (typed
                            (type #Star.Core.Int)
                            (get-slot line))
                        :suffix
                        :unary #++)
                    (set-slot column
                        (int 0))
                else
                    (set-slot lastWasCR
                        (bool false))))
            (else
                (if (typed
                    (type #Star.Core.Bool)
                    (send
                        (typed
                            (type #Star.Core.Bool)
                            (send
                                (typed
                                    (type #Star.Core.Bool)
                                    (send
                                        (int 31)
                                        :infix { :binary #< :from (type #Star.Core.Int) }
                                        (typed
                                            (type #Star.Core.Char)
                                            (get char))))
                                :infix { :binary #&& :from (type #Star.Core.Bool) }
                                (typed
                                    (type #Star.Core.Bool)
                                    (send
                                        (typed
                                            (type #Star.Core.Char)
                                            (get char))
                                        :infix { :binary #< :from (type #Star.Core.Char) }
                                        (int 127)))))
                        :infix { :binary #|| :from (type #Star.Core.Bool) }
                        (typed
                            (type #Star.Core.Bool)
                            (send
                                (typed
                                    (type #Star.Core.Char)
                                    (get char))
                                :infix { :binary #?= :from (type #Star.Core.Char) }
                                (char #"\t")))))
                    (send
                        (typed
                            (type #Star.Core.Int)
                            (get-slot column))
                        :suffix
                        :unary #++))
                (set-slot lastWasCR
                    (typed
                        (type #Star.Core.Bool)
                        (send
                            (typed
                                (type #Star.Core.Bool)
                                (get-slot lastWasCR))
                            :infix { :binary #&& :from (type #Star.Core.Bool) }
                            (bool false))))))))
*/