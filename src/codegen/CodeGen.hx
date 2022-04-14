package codegen;

import typing.*;
import Util.detuple;
import codegen.Opcode;

private enum Capture {
	CLocal(name: String, t: TypeRef);
	CAssign(name: String, t: TypeRef, assignTo: TExpr);
}

@:publicFields
@:build(util.Overload.build())
class CodeGen {
//

static final world = new World();


overload static function compile(ctx: GenCtx, stmts: TStmts): Opcodes {
	final res = [];
	
	for(stmt in stmts) {
		res.pushAll(compile(ctx, stmt));
	}

	return res;
}

overload static function compile(ctx: GenCtx, stmt: TStmt): Opcodes return stmt.s._match(
	at(SExpr(e)) => {
		compile(ctx, e, false);
	},

	at(SIf(cond, then, null)) => {
		compile(ctx, cond).concat([
			OIf(compile(ctx, then))
		]);
	},
	at(SIf(cond, then, orelse!!)) => {
		compile(ctx, cond).concat([
			OIfElse(
				compile(ctx, then),
				compile(ctx, orelse)
			)
		]);
	},

	at(SCase(cases, orelse)) => {
		function loop(i: Int) {
			final c = cases[i];
			final cthen = compile(ctx, c.then);
			return compile(ctx, c.cond).concat([
				if(i == cases.length-1) {
					orelse._andOr(oe => {
						OIfElse(
							cthen,
							compile(ctx, oe)
						);
					}, {
						OIf(cthen);
					});
				} else {
					OIfElse(cthen, loop(i + 1));
				}
			]);
		}

		loop(0);
	},

	at(SMatch(value, cases, orelse)) => {
		final ctx2 = ctx.inner();
		final matchLabel = ctx2.label();
		final exitMatch = OBreak(matchLabel);

		var res = compile(ctx2, value);

		for(c in cases) { final pattern = c.pattern, cond = c.cond, then = c.then;
			final caseLabel = ctx2.label();
			final exitCase = OIfNot([
				OBreak(caseLabel)
			]);

			// TODO: fix leaky stack
			res.push({
				final matchCtx = ctx2.inner();
				final captures = new Map<String, Capture>();
				final pat = compile(matchCtx, exitCase, captures, value.t.nonNull(), pattern, false, true, true);
	
				var case_: Opcodes = [ODup];
	
				compileCapturesPre(matchCtx, case_, captures);
				case_ = case_.concat(pat);
				cond._and(cond => {
					case_ = case_.concat(compile(matchCtx, cond));
					case_.push(exitCase);
				});
				compileCapturesPost(matchCtx, case_, captures);
				
				case_ = case_.concat(compile(matchCtx, then));
	
				if(!case_.last().match(ORet | ORetVoid | OBreak(_) | ONext(_) | OThrow(_) | ORethrow)) {
					case_.push(exitMatch);
				}
	
				ODo(caseLabel, case_);
			});
		}

		orelse._andOr(orelse => {
			res = res.concat(compile(ctx2, orelse));
		}, { // TODO: maybe remove?
			res.push(OStr("Match error!"));
			res.push(OThrow(stmt.orig.nonNull().mainSpan().display()));
		});

		[ODo(matchLabel, res)];
	},

	at(SMatchAt(value, pattern, cond, then, orelse)) => {
		final ctx2 = ctx.inner();
		final matchLabel = ctx2.label();
		final exitMatch = OBreak(matchLabel);
		
		final caseLabel = ctx2.label();
		final exitCase = OIfNot([
			OBreak(caseLabel)
		]);

		var res = compile(ctx2, value);

		res.push({
			final matchCtx = ctx2.inner();
			final captures = new Map<String, Capture>();
			final pat = compile(matchCtx, exitCase, captures, value.t.nonNull(), pattern, false, true, true);

			var case_: Opcodes = [];

			compileCapturesPre(matchCtx, case_, captures);
			case_ = case_.concat(pat);
			cond._and(cond => {
				case_ = case_.concat(compile(matchCtx, cond));
				case_.push(exitCase);
			});
			compileCapturesPost(matchCtx, case_, captures);
			
			case_ = case_.concat(compile(matchCtx, then));

			if(orelse != null && !case_.last().match(ORet | ORetVoid | OBreak(_) | ONext(_) | OThrow(_) | ORethrow)) {
				case_.push(exitMatch);
			}

			ODo(caseLabel, case_);
		});

		orelse._and(orelse => {
			res = res.concat(compile(ctx2, orelse));
		});

		[ODo(matchLabel, res)];
	},

	at(SWhile(cond, label, body)) => {
		final ctx2 = ctx.inner();
		final label2 = ctx2.loop(label);
		[OLoop(label2, (
			cond.e._match(
				at(EBool(true)) => compile(ctx2, body),
				_ => compile(ctx2, cond).concat([
					OIfElse(
						compile(ctx2, body),
						[OBreak(label2)]
					)
				])
			)
		))];
	},

	at(SDoWhile(body, label, cond)) => {
		final ctx2 = ctx.inner();
		final label2 = ctx2.loop(label);
		[OLoopThen(label2, (
			compile(ctx2, body)
		), (
			compile(ctx2, cond).concat([
				OIfNot([
					OBreak(label2)
				])
			])
		))];
	},

	at(SForIn(lpat, lpat2, inExpr, cond, label, body)) => {
		final label2 = ctx.loop(label);
		final exitLoop = OIfNot([
			OBreak(label2)
		]);
		var res = compile(ctx, inExpr);

		var kpat: Null<Pattern>, vpat: Pattern;
		lpat2._andOr(lpat2 => {
			kpat = lpat;
			vpat = lpat2;
		}, {
			kpat = null;
			vpat = lpat;
		});

		final etype = inExpr.t.nonNull();

		if(etype.hasParentType(Pass2.STD_Array) || etype.hasParentType(Pass2.STD_Str)) {
			final length = etype.findInstMember(/*BAD*/null, "length", false).nonNull();
			final buffer = etype.findInstMember(/*BAD*/null, "buffer", false).nonNull();

			final lenName = ctx.anon();
			final bufName = ctx.anon();
			final iname = kpat._andOr(kp => kp.p._match(
				at(PMy(name)) => name,
				at(PIgnore) => ctx.anon(),
				_ => throw "NYI"
			), {
				ctx.anon();
			});

			final intref = world.getTypeRef(Pass2.STD_Int);
			
			res.push(ODup);
			res.push(ONewLocal(lenName, intref));
			res.push(OGetMember(world.getInstID(length.getMember())));
			res.push(OSetLocal(lenName));

			res.push(ONewLocal(bufName, world.getTypeRef(buffer.retType().nonNull())));
			res.push(OGetMember(world.getInstID(buffer.getMember())));
			res.push(OSetLocal(bufName));

			res.push(ONewLocal(iname, intref));
			res.push(OInt32(0, true));
			res.push(OSetLocal(iname));

			final ctx2 = ctx.inner();
			var loop = [
				OGetLocal(iname),
				OGetLocal(lenName),
				ONative("i32_lt"),
				exitLoop
			];

			loop.push(OGetLocal(bufName));
			loop.push(OGetLocal(iname));
			loop.push(ONative("ptr_get_at"));
			
			final captures = new Map<String, Capture>();
			final pat = compile(ctx2, exitLoop, captures, /*TODO: cache this or smth*/ etype.iterElemType().nonNull(), vpat, true, true, true);

			compileCapturesPre(ctx2, loop, captures);
			loop = loop.concat(pat);
			cond._and(cond => {
				loop = loop.concat(compile(ctx2, cond));
				loop.push(exitLoop);
			});
			compileCapturesPost(ctx2, loop, captures);
			
			loop = loop.concat(compile(ctx2, body));

			res.push(OLoopThen(label2, loop, [
				OGetLocal(iname),
				ONative("i32_succ"),
				OSetLocal(iname)
			]));
		} else {
			final iterName = ctx.anon();

			kpat._andOr(kpat => {
				throw "TODO";
			}, {
				final elemType = etype.iterElemType().nonNull(); // TODO: cache this or smth
				final iterType = Pass2.STD_Iterator1.applyArgs([elemType]).nonNull();

				final casts = CastKind.reduceOverloads(
					etype.findCast(null, iterType, etype),
					etype,
					iterType
				)._match(
					at([]) => throw 'Cannot iterate on type `${etype.fullName()}`!',
					at(kinds = [_]) => kinds,
					at(kinds) => throw 'Cannot find specific iterator for type `${etype.fullName()}`!'
				);

				final iterNext = iterType.findSingleInst(/*BAD*/null, "next", Pass2.STD_Iterator1).nonNull();
				final iterRet = iterNext.retType().nonNull();
				final noneCase = iterRet.allTaggedCases().find(c -> c is SingleTaggedCase).nonNull();
				final noneCaseID = world.getID(noneCase);

				res = res.concat(compile(iterType, casts));
				res.push(ONewLocal(iterName, world.getTypeRef(iterType)));
				res.push(OSetLocal(iterName));

				final ctx2 = ctx.inner();
				var loop = [
					OGetLocal(iterName)
				];

				loop = loop.concat(compile(ctx2, iterNext, true));
				
				loop.push(ODup);
				loop.push(OKindID);
				loop.push(OTCaseID(world.getTypeRef(iterRet), noneCaseID));
				loop.push(ONative("tcaseid_eq"));
				loop.push(OIf([
					OBreak(label2)
				]));

				loop.push(OKindSlot(0));

				final captures = new Map<String, Capture>();
				final pat = compile(ctx2, exitLoop, captures, elemType, vpat, true, true, true);

				compileCapturesPre(ctx2, loop, captures);
				loop = loop.concat(pat);
				cond._and(cond => {
					loop = loop.concat(compile(ctx2, cond));
					loop.push(exitLoop);
				});
				compileCapturesPost(ctx2, loop, captures);

				loop = loop.concat(compile(ctx2, body));

				res.push(OLoop(label2, loop));
			});
		}

		ctx.popLoop(label);

		res;
	},

	at(SForRange(lvar, start, stop, by, cond, label, body)) => {
		final label2 = ctx.loop(label);
		var res: Opcodes = [];

		final lname = lvar._andOr(lv => lv.e._match(
			at(EVarDecl(name, _, _)) => name,
			_ => throw "NYI"
		), {
			ctx.anon();
		});

		detuple(@final [startKind, startExpr] = start);
		detuple(@final [stopKind, stopExpr] = stop);

		final startType = startExpr.t._or(throw "bad");
		final stopType = stopExpr.t._or(throw "bad");
		if(startType != stopType) throw "todo";

		res.push(ONewLocal(lname, world.getTypeRef(startType)));

		final downwards = (stopKind == LoopDownto);

		if(startType.isNative(NInt32)) {
			// Initialize loop var
			res = res.concat(compile(ctx, startExpr));
			if(startKind == LoopAfter) {
				res.push(ONative(downwards? "i32_pred" : "i32_succ"));
			}
			res.push(OSetLocal(lname));

			var stopOps = stopExpr.e._match(
				at(EName(_, _)) => compile(ctx, stopExpr),
				_ => {
					final name = ctx.anon();
					
					res.push(ONewLocal(name, world.getTypeRef(stopType)));
					if(stopKind == LoopTimes) {
						res.push(OGetLocal(lname));
						if(by != null) throw "NYI!";
						res = res.concat(compile(ctx, stopExpr));
						res.push(ONative("i32_add"));
					} else {
						res = res.concat(compile(ctx, stopExpr));
					}
					res.push(OSetLocal(name));

					[OGetLocal(name)];
				}
			);

			var byOp = by._and(e => {
				final byName = ctx.anon();
				final byType = e.t._or(throw "bad");

				res.push(ONewLocal(byName, world.getTypeRef(byType)));
				res = res.concat(compile(ctx, e));
				res.push(OSetLocal(byName));

				OGetLocal(byName);
			});

			// Loop head
			var loop = [OGetLocal(lname)];
			loop = loop.concat(stopOps);
			loop.push(ONative(
				stopKind._match(
					at(LoopTo | LoopTimes) => "i32_le",
					at(LoopUpto) => "i32_lt",
					at(LoopDownto) => "i32_ge"
				)
			));
			loop.push(OIfNot([
				OBreak(label2)
			]));
			cond._and(cond => {
				loop = loop.concat(compile(ctx, cond));
				loop.push(OIfNot([
					OBreak(label2)
				]));
			});

			loop = loop.concat(compile(ctx, body));

			// Loop tail
			final then = byOp._andOr(op => [
				OGetLocal(lname),
				op,
				ONative("i32_add"),
				OSetLocal(lname)
			], [
				OGetLocal(lname),
				ONative(downwards? "i32_prec" : "i32_succ"),
				OSetLocal(lname)
			]);

			// Loop
			res.push(OLoopThen(label2, loop, then));
		} else {
			throw "todo"+startType.getNative();
		}

		ctx.popLoop(label);

		res;
	},

	at(SDo(label, body)) => {
		final ctx2 = ctx.inner();
		final label2 = ctx2.loop(label);
		[ODo(label2, compile(ctx2, body))];
	},

	at(SReturn(null)) => [ORetVoid],
	at(SReturn(value!!)) => {
		compile(ctx, value).concat([
			ORet
		]);
	},

	at(SBreak(null)) => [OBreak(ctx.getLoop()._or(throw "Cannot break here!"))],
	at(SBreak(Left(depth))) => [OBreak(ctx.getLoop(depth)._or(throw "Cannot break here!"))],
	at(SBreak(Right(label))) => [OBreak(label)],

	at(SNext(null)) => [ONext(ctx.getLoop()._or(throw "Cannot continue here!"))],
	at(SNext(Left(depth))) => [ONext(ctx.getLoop(depth)._or(throw "Cannot continue here!"))],
	at(SNext(Right(label))) => [ONext(label)],

	at(SThrow(span, value)) => {
		compile(ctx, value).concat([
			OThrow(span.display())
		]);
	},

	at(STry(body, cases, orelse)) => {
		final tryOps = compile(ctx.inner(), body);
		var catchOps: Opcodes = [];

		final ctx2 = ctx.inner();
		final catchLabel = ctx2.label();
		final exitCatch = OBreak(catchLabel);

		for(c in cases) { final pattern = c.pattern, cond = c.cond, then = c.then;
			final caseLabel = ctx2.label();
			final exitCase = OIfNot([
				OBreak(caseLabel)
			]);

			catchOps.push({
				final catchCtx = ctx2.inner();
				final captures = new Map<String, Capture>();
				final pat = compile(catchCtx, exitCase, captures, {t: TBlank, span: null}, pattern, false, true, true);
	
				var case_: Opcodes = [ODup];
	
				compileCapturesPre(catchCtx, case_, captures);
				case_ = case_.concat(pat);
				cond._and(cond => {
					case_ = case_.concat(compile(catchCtx, cond));
					case_.push(exitCase);
				});
				compileCapturesPost(catchCtx, case_, captures);
				
				case_ = case_.concat(compile(catchCtx, then));
	
				if(!case_.last().match(ORet | ORetVoid | OBreak(_) | ONext(_) | OThrow(_) | ORethrow)) {
					case_.push(exitCatch);
				}
	
				ODo(caseLabel, case_);
			});
		}

		orelse._andOr(orelse => {
			// maybe ban this? idk
			catchOps = catchOps.concat(compile(ctx2, orelse));
		}, {
			catchOps.push(ORethrow);
		});

		[OTry(tryOps, catchOps)];
	}
);

static function getName(ctx: GenCtx, name: String, loc: Local, depth: Int): Opcodes {
	return loc._match(
		at(lf is Pass2.LocalField) => {
			final mem = lf.member;
			ctx.getThis(depth)._match(
				at(null) => {
					[mem.isStatic ? OGetStaticField(name) : OGetField(name)];
				},
				at({_1: thisName, _2: thisType}) => {
					if(mem.isStatic) [
						OGetStaticMember(
							thisType,
							world.getStaticID(mem)
						)
					] else [
						OGetLocal(thisName),
						OGetMember(
							world.getInstID(mem)
						)
					];
				}
			);
		},
		_ => {
			// TODO
			[OGetLocal(name)];
		}
	);
}

static function setName(name: String, loc: Local): Opcode {
	return loc._match(
		at(lf is Pass2.LocalField) => {
			lf.member.isStatic ? OSetStaticField(name) : OSetField(name);
		},
		_ => {
			// TODO
			OSetLocal(name);
		}
	);
}

static function setOrTeeName(name: String, loc: Local, wantValue: Bool): Opcode {
	return loc._match(
		at(lf is Pass2.LocalField) => {
			lf.member.isStatic
				? (wantValue ? OTeeStaticField(name) : OSetStaticField(name))
				: (wantValue ? OTeeField(name) : OSetField(name));
		},
		_ => {
			// TODO
			wantValue ? OTeeLocal(name) : OSetLocal(name);
		}
	);
}

overload static function compile(ctx: GenCtx, expr: TExpr, wantValue = true): Opcodes return expr.e._match(
	at(EName(name, loc)) => if(wantValue) {
		getName(ctx, name, loc, expr.orig.nonNull()._match(
			at(EName(_, _)) => 0,
			at(EObjMember(EWildcard(_), _)) => 1,
			_ => throw "NYI!"
		));
	} else [],

	at(ETag(_, _)) => throw "this should not be here!",

	at(EInt(int, exp)) => if(wantValue) {
		// TODO: merge with `Type <int>` compile logic
		[exp._andOr(
			e => ODec64(int, "0", e),
			OInt32(int, true)
		)];
	} else [],

	at(EDec(int, dec, exp)) => if(wantValue) {
		// TODO: merge with `Type <dec>` compile logic
		[ODec64(int, dec, exp)];
	} else [],

	at(EChar(char)) => if(wantValue) {
		// TODO: merge with `Type <dec>` compile logic
		[
			OInt8(char, false),
			ONativeCast(world.getTypeRef(Pass2.STD_Char))
		];
	} else [],

	at(EStr([])) => if(wantValue) [OStr("")] else [],
	at(EStr([PStr(str)])) => if(wantValue) [OStr(str)] else [],
	at(EStr(parts)) => {
		trace("TODO");
		[];
	},

	at(EBool(true)) => if(wantValue) [OTrue] else [],
	at(EBool(false)) => if(wantValue) [OFalse] else [],
	
	at(EArray(values)) => {
		final type = expr.t.nonNull();
		if(type.t.match(TMulti(_))) throw "type inference is not currently available for arrays!";
		var res = {
			final kind = type.findSingleStatic(/*BAD*/null, "new", type).nonNull();
			compile(ctx, type, kind, true);
		};

		for(value in values) {
			final overloads = MultiInstKind.reduceOverloads(
				type.findMultiInst(/*BAD*/null, ["add"], type),
				/*BAD*/null,
				type,
				[value]
			);

			res.push(ODup);
			res = res.concat(compile(ctx, overloads, [value], false));
		}
		
		res;
	},

	at(EHash(pairs)) => {
		trace("TODO");
		[];
	},

	at(ETuple(values)) => {
		final type = expr.t.nonNull();
		final typeref = world.getTypeRef(type);
		var res: Opcodes = [OInitClass(typeref)];
		
		final members = type.instMembers(type.t._match(
			at(TConcrete(decl) | TInstance(decl, _, _)) => decl,
			_ => throw "bad"
		)).filter(mem -> !mem.isStatic);

		if(members.length != values.length) throw "bad";

		members._for(i => mem, {
			res.push(ODup);
			res = res.concat(compile(ctx, values[i]));
			res.push(OSetMember(world.getInstID(mem)));
		});

		res;
	},

	at(EThis) => if(wantValue) {
		[ctx.getThis()._match(
			at(null) => OThis,
			at({_1: name, _2: type}) => OGetLocal(name)
		)];
	} else [],

	at(EWildcard) => throw "bad",

	at(EFunc(params, ret, body)) => {
		[];
	},

	at(EAnonArg(_, _)) => throw "bad",

	at(ELiteralCtor(type, {e: EInt(int, exp)})) => {
		// TODO: properly support 64-bit ints;
		inline function intVal() {
			return exp._andOr(
				e => {
					if(e < 0) throw "integer exponent cannot be under 0";
					int * Std.int(Math.pow(10, e));
				},
				int
			);
		}
		[type.getNative()._match(
			at(null) => throw "Cannot construct int literal from non-native type!",
			at(kind!!) => kind._match(
				at(NBool) => int._match( // ignore exp pls
					at(0) => OFalse,
					at(1) => OTrue,
					_ => throw "Invalid integer for bool ctor"
				),
				at(NInt8 | NUInt8) => OInt8(intVal(), kind == NInt8),
				at(NInt16 | NUInt16) => OInt16(intVal(), kind == NInt16),
				at(NInt32 | NUInt32) => OInt32(intVal(), kind == NInt32),
				at(NInt64 | NUInt64) => {
					OInt64(exp._andOr(
						e => haxe.Int64.parseString('${int}e$e'), // TODO: improve
						haxe.Int64.ofInt(int)
					), kind == NInt64);
				},
				at(NDec32) => ODec32(int, "0", exp),
				at(NDec64) => ODec64(int, "0", exp),
				_ => throw "Cannot construct int literal from non-numeric type!"
			)
		)];
	},
	at(ELiteralCtor(type, {e: EDec(int, dec, exp)})) => {
		[type.getNative()._match(
			at(null) => throw "Cannot construct dec literal from non-native type!",
			at(kind!!) => kind._match(
				at(NDec32) => ODec32(int, dec, exp),
				at(NDec64) => ODec64(int, dec, exp),
				_ => throw "Cannot construct dec literal from non-numeric type!"
			)
		)];
	},
	at(ELiteralCtor(type, {e: EArray(values)})) => {
		var res = {
			final kind = type.findSingleStatic(/*BAD*/null, "new", type).nonNull();
			compile(ctx, type, kind, true);
		};

		for(value in values) {
			final overloads = MultiInstKind.reduceOverloads(
				type.findMultiInst(/*BAD*/null, ["add"], type),
				/*BAD*/null,
				type,
				[value]
			);

			res.push(ODup);
			res = res.concat(compile(ctx, overloads, [value], false));
		}
		
		res;
	},
	at(ELiteralCtor(type, literal)) => {
		trace("TODO");
		[];
	},

	at(EParen([expr0])) => compile(ctx, expr0, wantValue),
	at(EParen(exprs)) => {
		final res = [];
		for(i in 0...exprs.length-1) {
			res.pushAll(compile(ctx, exprs[i], false));
		}
		res.concat(compile(ctx, exprs.last(), wantValue));
	},

	at(EBlock(stmts)) => [OBlock(compile(ctx, stmts))],

	at(ETypeMessage(type, msg)) => compile(ctx, type, msg, wantValue),

	at(ETypeCascade(type, cascades)) => {
		var res: Opcodes = [];

		for(cascade in cascades) {
			final wantValue = cascade.nested.length > 0;
			cascade.kind._match(
				at(Lazy(_)) => throw "bad",
				at(Member(msg)) => {
					res.pushAll(compile(ctx, type, msg, wantValue));
				},
				at(Message(msg)) => {
					res.pushAll(compile(ctx, type, msg, wantValue));
				},
				at(AssignMember(setMem, op, expr)) => {
					if(op != null) throw "todo";
					res.pushAll(compile(ctx, type, setMem, wantValue));
				},
				at(AssignMessage(setMsg, op, expr)) => {
					if(op != null) throw "todo";
					res.pushAll(compile(ctx, type, setMsg, wantValue));
				},
				at(StepMember(setMem, getMem, step)) => {
					res.push(ODup);
					res.pushAll(compile(ctx, type, getMem, true));
					res.pushAll(compilePrefix(step, true));
					res.pushAll(compile(ctx, type, [{kind: setMem, tctx: null}], [], wantValue));
				},
				at(StepMessage(setMsg, getMsg, step)) => {
					throw "todo";
				},
				at(Block(_, blk)) => {
					// TODO
					res.push(OBlock(
						compile(ctx.addThis("", world.getTypeRef(type)), blk)
					));
				}
			);

			if(wantValue) {
				for(c in cascade.nested) {
					res.push(ODup);
					compile(ctx, res, c);
				}

				res.push(OPop);
			}
		}

		res;
	},

	at(ETypeMember(type, kind)) => compile(ctx, type, kind, wantValue),

	at(EObjMessage(expr, msg)) => compile(ctx, expr).concat(compile(ctx, msg, wantValue)),

	at(EObjCascade(expr, cascades)) => {
		final res = compile(ctx, expr);

		for(cascade in cascades) {
			res.push(ODup);
			compile(ctx, res, cascade);
		}

		if(!wantValue) {
			res.push(OPop);
		}

		res;
	},

	at(EObjMember(expr, kind)) => compile(ctx, expr).concat(compile(ctx, kind, wantValue)),

	at(EObjLazyMember(_, _)) => throw "this should not be here!",

	at(EPrefix(kind, right)) => compile(ctx, right).concat(compilePrefix(kind, wantValue)),

	at(ELazyPrefix(_, _)) => throw "this should not be here!",

	at(ESuffix(left, kind)) => compile(ctx, left).concat(compileSuffix(kind, wantValue)),

	at(ELazySuffix(_, _)) => throw "this should not be here!",

	at(EInfix(left, kinds, right)) => compile(ctx, left).concat(compile(ctx, kinds, right, wantValue)),

	at(ELazyInfix(_, _, _)) => throw "this should not be here!",

	at(EInfixChain(left, chain)) => throw "todo",

	at(EVarDecl(name, _, value)) => {
		final res: Opcodes = [ONewLocal(name, world.getTypeRef(expr.t.nonNull()))];
		value._andOr(v => {
			res
			.concat(compile(ctx, v))
			.concat([
				wantValue ? OTeeLocal(name) : OSetLocal(name)
			]);
		}, {
			res;
		});
	},

	at(ESetName(name, loc, value)) => {
		compile(ctx, value).concat([
			setOrTeeName(name, loc, wantValue)
		]);
	},


	at(EDestructure(pattern, value)) => {
		if(wantValue) throw "absolutely not";

		pattern.p._match(
			at(PTuple(patterns)) => {
				var res: Opcodes = [];
				
				value.e._match(
					at(ETuple(values)) => {
						if(patterns.length != values.length) throw "bad";
						values._for(i => v, {
							patterns[i].p._match(
								at(PIgnore) => continue,
								_ => {
									res = res.concat(
										compile(ctx, v)
									);
								}
							);
						});
					},
					_ => {
						res = res.concat(compile(ctx, value));

						final type = value.t.nonNull();
						if(type.unifyWithType(Pass2.STD_Tuple) == null) throw "bad";

						final members = type.instMembers(type.t._match(
							at(TConcrete(decl) | TInstance(decl, _, _)) => decl,
							_ => throw "bad"
						)).filter(mem -> !mem.isStatic);

						if(members.length != patterns.length) throw "bad";

						patterns._for(i => p, { final mem = members[i];
							p.p._match(
								at(PIgnore) => continue,
								_ => {
									final memID = world.getInstID(mem);
									res.push(ODup);
									res.push(OGetMember(memID));
								}
							);
						});
					}
				);

				final lenp = patterns.length;
				for(i in 0...lenp) { final pat = patterns[lenp - i - 1]; pat.p._match(
					at(PExpr(expr)) => expr.e._match(
						at(EName(name, loc)) => {
							res.push(setName(name, loc));
						},

						at(ELiteralCtor(type, literal)) => throw "NYI",
						at(ETypeMessage(type, msg)) => throw "NYI",
						at(ETypeMember(type, kind)) => throw "NYI",
						at(EObjCascade(expr, cascades)) => throw "NYI",
						at(EObjMember(obj, kind)) => throw "NYI",
						
						at(EVarDecl(name, _, null)) => {
							res.push(ONewLocal(name, world.getTypeRef(expr.t.nonNull())));
							res.push(OSetLocal(name));
						},

						_ => throw "bad"
					),

					at(PExtractor(_)) => throw "???",

					at(PIgnore) => continue,

					at(PMy(name)) => {
						res.push(ONewLocal(name, world.getTypeRef(expr.t.nonNull())));
						res.push(OSetLocal(name));
					},

					at(PMyType(name, type)) => {
						// TODO
						final typeref = world.getTypeRef(type.nonNull());
						
						res.push(ONewLocal(name, typeref));
						res.push(ODup);
						res.push(OOfType(typeref));
						res.push(OIfNot([
							OStr("Match error!"), // TODO
							OThrow(pat.orig._andOr(u => u.mainSpan().display(), "???"))
						]));
						res.push(ONativeCast(typeref)); // TODO?
						res.push(OSetLocal(name));
					},

					_ => throw "NYI"
				); }

				res;
			},
			_ => {
				[];
			}
		);
	},

	at(EInitThis(type, msg)) => {
		final res = compile(ctx, type, msg, true);
		res.setLast(res.last()._match(
			at(OSend_IS(t, id)) => OInitThis_S(t, id),
			at(OSend_IM(t, id, ctx)) => OInitThis_M(t, id, ctx),
			_ => throw "bad"
		));
		res;
	},

	at(EInline(expr)) => {
		// no-op for now
		compile(ctx, expr, wantValue);
	},

	at(EKindId(expr)) => {
		final res = compile(ctx, expr, wantValue);
		if(wantValue) {
			res.concat([OKindID]);
		} else {
			res;
		}
	},

	at(EKindSlot(expr, i)) => {
		final res = compile(ctx, expr, wantValue);
		if(wantValue) {
			res.concat([OKindSlot(i)]);
		} else {
			res;
		}
	},

	at(EInvalid) => throw "this should not be here!",

	at(EPatternType(type)) => throw "bad"
);

// TODO: fix leaky stack
overload static function compile(
	ctx: GenCtx, exitCase: Opcode, captures: Map<String, Capture>, target: Type, pattern: Pattern,
	isRhs: Bool, allowCaptures: Bool, uniqueCaptures: Bool
) {
	pattern.p._match(
		at(PExpr(expr)) => {
			final eq = BinaryOpKind.simplify(
				BinaryOpKind.reduceOverloads(target.findBinaryOp(null, Eq, target), target, expr)
			);
			
			final res = compile(ctx, eq, expr, true);
			res.push(exitCase);
			return res;
		},

		at(PExtractor(extractor)) => throw "bad",

		at(PExtractMessage(msg)) => {
			final tpattern = pattern.t.nonNull();
			//if(tpattern.isNative(NVoid)) throw "bad";
			if(isRhs && !tpattern.isNative(NBool)) {
				throw 'Expected logical value, got `${tpattern.fullName()}` instead';
			}

			final res = compile(ctx, msg, true);
			if(!isRhs) {
				res.push(exitCase);
			}
			return res;
		},

		at(PIgnore) => {
			return if(isRhs) [] else [OPop];
		},

		at(PMy(name)) => {
			captures[name]._match(
				at(null) => {
					captures[name] = CLocal(name, world.getTypeRef(target));
					return [OSetLocal(name)];
				},
				at(CLocal(_, type) | CAssign(_, type, _)) => {
					if(uniqueCaptures) throw "bad";
					if(!type.equals(world.getTypeRef(target))) throw "bad";
					return [OSetLocal(name)];
				}
			);
		},

		at(PMyType(name, type)) => {
			final typeref = world.getTypeRef(type);
			//final targetref = world.getTypeRef(target);
			captures[name]._match(
				at(null) => {
					captures[name] = CLocal(name, typeref);
					return [
						ODup,
						OOfType(typeref),
						exitCase,
						if(type.hasParentType(target)) ODowncast(typeref) else OUpcast(typeref),
						OSetLocal(name)
					];
				},
				at(CLocal(_, type2) | CAssign(_, type2, _)) => {
					if(uniqueCaptures) throw "bad";
					if(!type2.equals(typeref)) throw "bad";
					return [
						ODup,
						OOfType(typeref),
						exitCase,
						if(type.hasParentType(target)) ODowncast(typeref) else OUpcast(typeref),
						OSetLocal(name)
					];
				}
			);
		},

		at(PType(type)) => {
			final typeref = world.getTypeRef(type);
			return if(isRhs) [
				ODup,
				OOfType(typeref),
				exitCase,
				if(type.hasParentType(target)) ODowncast(typeref) else OUpcast(typeref)
			] else [
				OOfType(typeref),
				exitCase
			];
		},

		at(PAll(patterns)) => {
			var res: Opcodes = [];

			for(pat in patterns) {
				res.push(ODup);
				res = res.concat(
					compile(ctx, exitCase, captures, target, pat, isRhs, allowCaptures, true)
				);
			}

			if(!isRhs) res.push(OPop);

			return res;
		},

		at(PAny(patterns)) => {
			final label = ctx.label();
			final exitAlt = OIf([
				OBreak(label)
			]);
			var res: Opcodes = [];

			for(pat in patterns) {
				res.push(ODup);
				res = res.concat(
					compile(ctx, exitAlt, captures, target, pat, isRhs, allowCaptures, false)
				);
			}

			if(!isRhs) res.push(OPop);

			exitCase._match(
				at(OIfNot(ops)) => res = res.concat(ops),
				_ => {
					res.push(OFalse);
					res.push(exitCase);
				}
			);

			return [ODo(label, res)];
		},

		at(POne(patterns)) => {
			throw "TODO";
		},

		at(PNone(patterns)) => {
			final exitAlt = exitCase._match(
				at(OIfNot(ops)) => OIf(ops),
				at(OIf(ops)) => OIfNot(ops),
				_ => throw "TODO"
			);
			var res: Opcodes = [];

			for(pat in patterns) {
				res.push(ODup);
				res = res.concat(
					compile(ctx, exitAlt, captures, target, pat, false, false, false)
				);
			}

			if(!isRhs) res.push(OPop);

			return res;
		},

		at(PNot(pattern2)) => {
			final exitNot = exitCase._match(
				at(OIfNot(ops)) => OIf(ops),
				at(OIf(ops)) => OIfNot(ops),
				_ => throw "TODO"
			);

			return compile(ctx, exitNot, captures, target, pattern2, false, false, false);
		},

		at(PBoundsMin(min, bound, pattern2)) => {
			var res: Opcodes = if(isRhs) [ODup] else [];

			min.p._match(
				at(PExpr(expr)) => {
					final op: BinaryOp = bound._match(
						at(Inclusive) => Ge,
						at(Exclusive) => Gt
					);
					final cmp = BinaryOpKind.simplify(
						BinaryOpKind.reduceOverloads(target.findBinaryOp(null, op, target), target, expr)
					);
					
					res = res.concat(compile(ctx, cmp, expr, true));
					res.push(exitCase);
				},
				at(PTypeValueCase(type, valueCase)) => {
					res.push(OKindID);
					res.push(OVCaseID(world.getTypeRef(type), world.getID(valueCase)));
					res.push(ONative(bound._match(
						at(Inclusive) => "vcaseid_ge",
						at(Exclusive) => "vcaseid_gt"
					)));
					res.push(exitCase);
				},
				at(PTypeTaggedCaseSingle(type, taggedCase)) => {
					res.push(OKindID);
					res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
					res.push(ONative(bound._match(
						at(Inclusive) => "tcaseid_ge",
						at(Exclusive) => "tcaseid_gt"
					)));
					res.push(exitCase);
				},
				at(PTypeTaggedCaseMulti(type, taggedCase, args)) => {
					if(!args.every(a -> a.p.match(PIgnore))) throw "bad";
					
					res.push(OKindID);
					res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
					res.push(ONative(bound._match(
						at(Inclusive) => "tcaseid_ge",
						at(Exclusive) => "tcaseid_gt"
					)));
					res.push(exitCase);
				},
				_ => throw "TODO"
			);

			res = res.concat(
				compile(ctx, exitCase, captures, target, pattern2, true, allowCaptures, uniqueCaptures)
			);

			return res;
		},

		at(PBoundsMax(pattern2, max, bound)) => {
			var res: Opcodes = if(isRhs) [ODup] else [];

			max.p._match(
				at(PExpr(expr)) => {
					final op: BinaryOp = bound._match(
						at(Inclusive) => Le,
						at(Exclusive) => Lt
					);
					final cmp = BinaryOpKind.simplify(
						BinaryOpKind.reduceOverloads(target.findBinaryOp(null, op, target), target, expr)
					);
					
					res = res.concat(compile(ctx, cmp, expr, true));
					res.push(exitCase);
				},
				at(PTypeValueCase(type, valueCase)) => {
					res.push(OKindID);
					res.push(OVCaseID(world.getTypeRef(type), world.getID(valueCase)));
					res.push(ONative(bound._match(
						at(Inclusive) => "vcaseid_le",
						at(Exclusive) => "vcaseid_lt"
					)));
					res.push(exitCase);
				},
				at(PTypeTaggedCaseSingle(type, taggedCase)) => {
					res.push(OKindID);
					res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
					res.push(ONative(bound._match(
						at(Inclusive) => "tcaseid_le",
						at(Exclusive) => "tcaseid_lt"
					)));
					res.push(exitCase);
				},
				at(PTypeTaggedCaseMulti(type, taggedCase, args)) => {
					if(!args.every(a -> a.p.match(PIgnore))) throw "bad";
					
					res.push(OKindID);
					res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
					res.push(ONative(bound._match(
						at(Inclusive) => "tcaseid_le",
						at(Exclusive) => "tcaseid_lt"
					)));
					res.push(exitCase);
				},
				_ => throw "TODO"
			);

			res = res.concat(
				compile(ctx, exitCase, captures, target, pattern2, true, allowCaptures, uniqueCaptures)
			);

			return res;
		},

		at(PBoundsMinMax(min, minBound, pattern2, max, maxBound)) => {
			var res: Opcodes = if(isRhs) [ODup, ODup] else [ODup];

			min.p._match(
				at(PExpr(expr)) => {
					final op: BinaryOp = minBound._match(
						at(Inclusive) => Ge,
						at(Exclusive) => Gt
					);
					final cmp = BinaryOpKind.simplify(
						BinaryOpKind.reduceOverloads(target.findBinaryOp(null, op, target), target, expr)
					);
					
					res = res.concat(compile(ctx, cmp, expr, true));
					res.push(exitCase);
				},
				at(PTypeValueCase(type, valueCase)) => {
					res.push(OKindID);
					res.push(OVCaseID(world.getTypeRef(type), world.getID(valueCase)));
					res.push(ONative(minBound._match(
						at(Inclusive) => "vcaseid_ge",
						at(Exclusive) => "vcaseid_gt"
					)));
					res.push(exitCase);
				},
				at(PTypeTaggedCaseSingle(type, taggedCase)) => {
					res.push(OKindID);
					res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
					res.push(ONative(minBound._match(
						at(Inclusive) => "tcaseid_ge",
						at(Exclusive) => "tcaseid_gt"
					)));
					res.push(exitCase);
				},
				at(PTypeTaggedCaseMulti(type, taggedCase, args)) => {
					if(!args.every(a -> a.p.match(PIgnore))) throw "bad";
					
					res.push(OKindID);
					res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
					res.push(ONative(minBound._match(
						at(Inclusive) => "tcaseid_ge",
						at(Exclusive) => "tcaseid_gt"
					)));
					res.push(exitCase);
				},
				_ => throw "TODO"
			);

			max.p._match(
				at(PExpr(expr)) => {
					final op: BinaryOp = maxBound._match(
						at(Inclusive) => Le,
						at(Exclusive) => Lt
					);
					final cmp = BinaryOpKind.simplify(
						BinaryOpKind.reduceOverloads(target.findBinaryOp(null, op, target), target, expr)
					);
					
					res = res.concat(compile(ctx, cmp, expr, true));
					res.push(exitCase);
				},
				at(PTypeValueCase(type, valueCase)) => {
					res.push(OKindID);
					res.push(OVCaseID(world.getTypeRef(type), world.getID(valueCase)));
					res.push(ONative(maxBound._match(
						at(Inclusive) => "vcaseid_le",
						at(Exclusive) => "vcaseid_lt"
					)));
					res.push(exitCase);
				},
				at(PTypeTaggedCaseSingle(type, taggedCase)) => {
					res.push(OKindID);
					res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
					res.push(ONative(maxBound._match(
						at(Inclusive) => "tcaseid_le",
						at(Exclusive) => "tcaseid_lt"
					)));
					res.push(exitCase);
				},
				at(PTypeTaggedCaseMulti(type, taggedCase, args)) => {
					if(!args.every(a -> a.p.match(PIgnore))) throw "bad";
					
					res.push(OKindID);
					res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
					res.push(ONative(maxBound._match(
						at(Inclusive) => "tcaseid_le",
						at(Exclusive) => "tcaseid_lt"
					)));
					res.push(exitCase);
				},
				_ => throw "TODO"
			);

			res = res.concat(
				compile(ctx, exitCase, captures, target, pattern2, true, allowCaptures, uniqueCaptures)
			);

			return res;
		},

		at(PAssignPattern(assign, pattern2)) => {
			var res = compile(ctx, exitCase, captures, target, pattern2, true, true, true);

			res = res.concat(
				compileDestructure(ctx, exitCase, captures, assign, pattern2.t.nonNull())
			);

			return res;
		},

		at(PArray(patterns)) => {
			throw "TODO";
		},

		at(PTypeArray(type, patterns)) => {
			throw "TODO";
		},

		at(PTuple(patterns)) => {
			var res: Opcodes = [];
			
			if(target.unifyWithType(Pass2.STD_Tuple) == null) throw "bad";

			final members = target.instMembers(target.t._match(
				at(TConcrete(decl) | TInstance(decl, _, _)) => decl,
				_ => throw "bad"
			)).filter(mem -> !mem.isStatic);

			if(members.length != patterns.length) throw "bad";

			patterns._for(i => pat, { final mem = members[i];
				pat.p._match(
					at(PIgnore) => continue,
					_ => {
						final memID = world.getInstID(mem);
						res.push(ODup);
						res.push(OGetMember(memID));
						res = res.concat(
							compile(ctx, exitCase, captures, /* TODO */ mem.type.nonNull().getFrom(target), pat, isRhs, allowCaptures, uniqueCaptures)
						);
					}
				);
			});

			if(!isRhs) res.push(OPop);

			return res;
		},

		at(PTypeTuple(type, patterns)) => {
			throw "TODO";
		},

		at(PTypeMembers(type, members)) => {
			if(members.length == 0) throw "bad";

			var res: Opcodes = [];

			if(target.strictUnifyWithType(type) == null) {
				final typeref = world.getTypeRef(type);

				res.push(ODup);
				res.push(OOfType(typeref));
				res.push(exitCase);
				res.push(if(type.hasParentType(target)) ODowncast(typeref) else OUpcast(typeref));
			}

			for(m in members) { detuple(@final [mem, pat] = m);
				res.push(ODup);
				res = res.concat(
					compile(ctx, exitCase, captures, /* TODO */ mem.type.nonNull().getFrom(target).getFrom(type), pat, isRhs, allowCaptures, uniqueCaptures)
				);
			}

			if(!isRhs) res.push(OPop);
			
			return res;
		},

		at(PTypeValueCase(type, valueCase)) => {
			var res: Opcodes = if(isRhs) [ODup] else [];
			
			res.push(OVCaseID(world.getTypeRef(type), world.getID(valueCase)));
			res.push(ONative("vcaseid_eq"));
			res.push(exitCase);

			return res;
		},

		at(PTypeTaggedCaseSingle(type, taggedCase)) => {
			var res: Opcodes = if(isRhs) [ODup] else [];
			
			res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
			res.push(ONative("tcaseid_eq"));
			res.push(exitCase);

			return res;
		},

		at(PTypeTaggedCaseMulti(type, taggedCase, args)) => {
			var res: Opcodes = if(isRhs) [ODup] else [];
			final hasPatterns = args.some(a -> !a.p.match(PIgnore));

			if(hasPatterns) {
				res.push(ODup);
			}
			
			res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
			res.push(ONative("tcaseid_eq"));
			res.push(exitCase);

			if(hasPatterns) args._for(i => arg, if(!arg.p.match(PIgnore)) {
				final argType = taggedCase.params[i].type.getFrom(type);

				res.push(ODup);
				res.push(OKindSlot(i));
				res = res.concat(
					compile(ctx, exitCase, captures, argType, arg, false, true, true)
				);
			});

			return res;
		},

		at(PTypeTaggedCaseMembersSingle(type, taggedCase, members)) => {
			throw "TODO";
		},

		at(PTypeTaggedCaseMembersMulti(type, taggedCase, args, members)) => {
			throw "TODO";
		},

		at(PExtractFrom(extract, from)) => {
			throw "TODO";
		},

		at(PExcludeFrom(exclude, from)) => {
			throw "TODO";
		},

		at(PComplement(complement)) => {
			throw "TODO";
		},

		at(PSpread(_) | POptional(_)) => throw "bad"
	);
}

static function compileDestructure(ctx: GenCtx, exitCase: Opcode, captures: Map<String, Capture>, lhs: Pattern, rhs: Type) {
	final rhsref = world.getTypeRef(rhs);
	lhs.p._match(
		at(PExpr(expr)) => {
			final anon = ctx.anon();
			
			captures[anon] = CAssign(anon, rhsref, expr);

			return [OSetLocal(anon)];
		},

		at(PExtractor(_)) => throw "???",

		at(PIgnore) => return [OPop],

		at(PMy(name)) => {
			if(captures.exists(name)) throw "bad";
			captures[name] = CLocal(name, rhsref);

			return [OSetLocal(name)];
		},

		at(PMyType(name, type)) => {
			// TODO
			final typeref = world.getTypeRef(type.nonNull());

			if(captures.exists(name)) throw "bad";
			captures[name] = CLocal(name, typeref);
			
			return [
				ODup,
				OOfType(typeref),
				exitCase,
				if(type.hasParentType(rhs)) ODowncast(typeref) else OUpcast(typeref),
				OSetLocal(name)
			];
		},

		at(PTuple(patterns)) => {
			var res: Opcodes = [];
				
			if(rhs.unifyWithType(Pass2.STD_Tuple) == null) throw "bad";

			final members = rhs.instMembers(rhs.t._match(
				at(TConcrete(decl) | TInstance(decl, _, _)) => decl,
				_ => throw "bad"
			)).filter(mem -> !mem.isStatic);

			if(members.length != patterns.length) throw "bad";

			patterns._for(i => p, { final mem = members[i];
				p.p._match(
					at(PIgnore) => continue,
					_ => {
						final memID = world.getInstID(mem);
						res.push(ODup);
						res.push(OGetMember(memID));
					}
				);
			});

			final lenp = patterns.length;
			for(i in 0...lenp) { final pat = patterns[lenp - i - 1];
				if(pat.p.match(PIgnore)) continue;
				res = res.concat(compileDestructure(ctx, exitCase, captures, pat, /* TODO */ members[i].type.nonNull().getFrom(rhs)));
			}

			return res;
		},

		_ => throw "TODO"
	);
}

static function compileCapturesPre(ctx: GenCtx, res: Opcodes, captures: Map<String, Capture>) {
	if(captures.size() == 0) return;
	
	for(capture in captures) capture._match(
		at(CLocal(name, t) | CAssign(name, t, _)) => {
			res.push(ONewLocal(name, t));
		}
	);
}
static function compileCapturesPost(ctx: GenCtx, res: Opcodes, captures: Map<String, Capture>) {
	if(captures.size() == 0) return;
	
	for(capture in captures) capture._match(
		at(CLocal(_, _)) => {},
		at(CAssign(name, t, expr)) => {
			res.push(OGetLocal(name));
			expr.e._match(
				at(EName(name, loc)) => {
					res.push(setName(name, loc));
				},
			
				at(ELiteralCtor(type, literal)) => throw "NYI",
				at(ETypeMessage(type, msg)) => throw "NYI",
				at(ETypeMember(type, kind)) => throw "NYI",
				at(EObjCascade(expr, cascades)) => throw "NYI",
				at(EObjMember(obj, kind)) => throw "NYI",
				
				at(EVarDecl(_, _, _)) => throw "???",
			
				_ => throw "bad"
			);
		}
	);
}
/*

*/

overload static function compile(ctx: GenCtx, res: Opcodes, cascade: ObjCascade) {
	final wantValue = cascade.nested.length > 0;
	cascade.kind._match(
		at(Lazy(_)) => throw "bad",
		at(Member(msg)) => {
			res.pushAll(compile(ctx, msg, wantValue));
		},
		at(Message(msg)) => {
			res.pushAll(compile(ctx, msg, wantValue));
		},
		at(AssignMember(setMem, op, expr)) => {
			if(op != null) throw "todo";
			res.pushAll(compile(ctx, setMem, wantValue));
		},
		at(AssignMessage(setMsg, op, expr)) => {
			if(op != null) throw "todo";
			res.pushAll(compile(ctx, setMsg, wantValue));
		},
		at(StepMember(setMem, getMem, step)) => {
			res.push(ODup);
			res.pushAll(compile(ctx, getMem, true));
			res.pushAll(compilePrefix(step, true));
			res.pushAll(compile(ctx, [{kind: setMem, tctx: null}], [], wantValue));
		},
		at(StepMessage(setMsg, getMsg, step)) => {
			throw "todo";
		},
		at(Block({thisType: type}, blk)) => {
			// TODO
			final anon = ctx.anon();
			final typeref = world.getTypeRef(type);
			res.push(OBlock(
				[
					ONewLocal(anon, typeref),
					OSetLocal(anon)
				].concat(
					compile(ctx.addThis(anon, typeref), blk)
				)
			));
		}
	);

	if(wantValue) {
		for(c in cascade.nested) {
			res.push(ODup);
			compile(ctx, res, c);
		}

		res.push(OPop);
	}
}

overload static function compile(ctx: GenCtx, type: Type, msg: TypeMessage, wantValue: Bool) return msg._match(
	at(Single(kind)) => compile(ctx, type, kind, wantValue),
	at(Multi(candidates, labels, args)) => compile(ctx, type, candidates, args, wantValue),
	at(Super(parent, msg)) => {
		trace(parent);
		[];
	}
);

overload static function compile(ctx: GenCtx, msg: ObjMessage, wantValue: Bool) return msg._match(
	at(Lazy(_)) => throw "bad",
	at(Single(kind)) => compile(ctx, kind, wantValue),
	at(Multi(candidates, labels, args)) => compile(ctx, candidates, args, wantValue),
	at(Cast(target, candidates)) => compile(target, candidates),
	at(Super(parent, msg)) => {
		trace(parent);
		[];
	}
);

overload static function compile(ctx: GenCtx, type: Type, kind: SingleStaticKind, wantValue: Bool): Opcodes return kind._match(
	at(SSInit(init)) => {
		init.native._and(native => native._match(
			at(None) => throw "bad",
			at(Some({name: name})) => {
				return [ONative(name)];
			}
		));

		final id = world.getID(init);
		final res: Opcodes = [OSend_IS(world.getTypeRef(type), id)];
		if(!wantValue) {
			res.push(OPop);
		}
		res;
	},
	at(SSMultiInit(init)) => {
		var res = [];

		for(param in init.params) {
			param.tvalue._andOr(tvalue => {
				// TODO?
				res = res.concat(compile(ctx, tvalue));
			}, {
				throw "bad";
			});
		}

		final id = world.getID(init);
		res.push(OSend_IM(world.getTypeRef(type), id));
		if(!wantValue) {
			res.push(OPop);
		}
		res;
	},
	at(SSMethod(mth)) => {
		mth.native._and(native => native._match(
			at(None) => throw "bad",
			at(Some({name: name})) => {
				return [ONative(name)];
			}
		));

		final id = world.getID(mth);
		final res: Opcodes = [OSend_SS(world.getTypeRef(type), id)];
		if(!wantValue && mth.ret._andOr(ret => ret != Pass2.STD_Void.thisType, false)) {
			res.push(OPop);
		}
		res;
	},
	at(SSMultiMethod(mth)) => {
		var res = [];

		for(param in mth.params) {
			param.tvalue._andOr(tvalue => {
				// TODO?
				res = res.concat(compile(ctx, tvalue));
			}, {
				throw "bad";
			});
		}

		final id = world.getID(mth);
		res.push(OSend_MS(world.getTypeRef(type), id));
		if(!wantValue && mth.ret._andOr(ret => ret != Pass2.STD_Void.thisType, false)) {
			res.push(OPop);
		}
		res;
	},
	at(SSMember(mem)) => {
		final id = world.getInstID(mem);
		[OGetStaticMember(world.getTypeRef(type), id)];
	},
	at(SSTaggedCase(c)) => {
		final id = world.getID(c);
		[OInitTKind(world.getTypeRef(type), id)];
	},
	at(SSTaggedCaseAlias(c)) => {
		[];
	},
	at(SSValueCase(c)) => {
		final id = world.getID(c);
		[OInitVKind(world.getTypeRef(type), id)];
	},
	at(SSFromTypevar(_, _, _, _)) => {
		[];
	},
	at(SSFromParent(parent, kind2)) => {
		// TODO
		type=type.getMostSpecific().simplify(); // FIX
		final res = compile(ctx, /*parent.getFrom(type)*/type, kind2, wantValue);
		/*if(wantValue) {
			final typeref = world.getTypeRef(type);
			kind2._match(
				at(SSInit(_) | SSMultiInit(_)) => res.push(ONativeCast(typeref)),
				at(SSTaggedCase(_) | SSTaggedCaseAlias(_) | SSValueCase(_)) => res.push(ODowncast(typeref)),
				at(SSFromParent(_, kind3)) => throw "NYI!",
				_ => {}
			);
		}*/
		res;
	}
);

overload static function compile(ctx: GenCtx, kind: SingleInstKind, wantValue: Bool): Opcodes return kind._match(
	at(SIMethod(mth)) => {
		mth.native._and(native => native._match(
			at(None) => throw "bad",
			at(Some({name: name})) => {
				return [ONative(name)];
			}
		));

		final id = world.getID(mth);
		final res: Opcodes = [mth.typedBody != null ? OSend_SI(id) : OSendDynamic_SI(id)];
		if(!wantValue && mth.ret._andOr(ret => ret != Pass2.STD_Void.thisType, false)) {
			res.push(OPop);
		}
		res;
	},
	at(SIMultiMethod(mth)) => {
		var res = [];

		for(param in mth.params) {
			param.tvalue._andOr(tvalue => {
				// TODO?
				res = res.concat(compile(ctx, tvalue));
			}, {
				throw "bad";
			});
		}

		final id = world.getID(mth);
		res.push(mth.typedBody != null ? OSend_MI(id) : OSendDynamic_MI(id));
		if(!wantValue && mth.ret._andOr(ret => ret != Pass2.STD_Void.thisType, false)) {
			res.push(OPop);
		}
		res;
	},
	at(SIMember(mem)) => {
		final id = world.getInstID(mem);
		[OGetMember(id)];
	},
	at(SIFromTypevar(_, _, _, _)) => {
		[];
	},
	at(SIFromParent(parent, kind)) => {
		[];
	}
);

overload static function compile(ctx: GenCtx, type: Type, candidates: Array<TypeMessage.TypeMultiCandidate>, args: Array<TExpr>, wantValue: Bool): Opcodes {
	return candidates._match(
		at([]) => throw "bad",
		at([{kind: kind, tctx: tctx}]) => {
			// TODO: generic type instanciation from type inference
			/*tctx._and(tctx => {
				type = type.getInTCtx(tctx);
			});*/
			final typeref = world.getTypeRef(type);
			kind._match(
				at(MSInit(init = {isMacro: true}, _)) => {
					throw "NYI";
				},
				at(MSInit(init, null)) => {
					init.native._and(native => native._match(
						at(None) => throw "bad",
						at(Some({name: name})) => {
							var res = [];
							for(arg in args) {
								res = res.concat(compile(ctx, arg));
							}
							res.push(ONative(name));
							return res;
						}
					));

					final ictx = tctx._andOr(tctx => (
						if(tctx.size() > 0) {
							final _ictx = new TVarInstCtx();
							for(tv => t in tctx) {
								_ictx[world.getTVar(tv)] = world.getTypeRef(t);
							}
							_ictx;
						} else
							null
					), null);

					final id = world.getID(init);
					var res = [];
					for(arg in args) {
						res = res.concat(compile(ctx, arg));
					}
					res.push(OSend_MS(typeref, id, ictx));
					if(!wantValue) {
						res.push(OPop);
					}
					res;
				},
				at(MSInit(init, partial!!)) => {
					[];
				},
				
				at(MSMethod(mth = {isMacro: true}, _)) => {
					throw "NYI";
				},
				at(MSMethod(mth, null)) => {
					mth.native._and(native => native._match(
						at(None) => throw "bad",
						at(Some({name: name})) => {
							var res = [];
							for(arg in args) {
								res = res.concat(compile(ctx, arg));
							}
							res.push(ONative(name));
							return res;
						}
					));

					final ictx = tctx._andOr(tctx => (
						if(tctx.size() > 0) {
							final _ictx = new TVarInstCtx();
							for(tv => t in tctx) {
								_ictx[world.getTVar(tv)] = world.getTypeRef(t);
							}
							_ictx;
						} else
							null
					), null);

					final id = world.getID(mth);
					var res = [];
					for(arg in args) {
						res = res.concat(compile(ctx, arg));
					}
					res.push(OSend_MS(typeref, id, ictx));
					if(!wantValue && mth.ret._andOr(ret => ret != Pass2.STD_Void.thisType, false)) {
						res.push(OPop);
					}
					res;
				},
				at(MSMethod(mth, partial!!)) => {
					[];
				},

				at(MSMember(mem)) => {
					final id = world.getInstID(mem);
					var res: Opcodes = if(args.length == 1) compile(ctx, args[0]) else [];
					res.push(wantValue ? OTeeStaticMember(typeref, id) : OSetStaticMember(typeref, id));
					res;
				},

				at(MSMemberwiseInit(ms)) => {
					var res = [OInitClass(typeref)];

					ms._for(i => mem, {
						final id = world.getInstID(mem);

						res.push(ODup);
						res = res.concat(compile(ctx, args[i]));
						res.push(OSetMember(id));
					});

					res;
				},

				at(MSTaggedCase([], c, ms2, null)) => {
					var res: Opcodes = [];

					var i = 0;
					final endInit = args.length - ms2.length;
					while(i < endInit) {
						res = res.concat(compile(ctx, args[i]));
						i++;
					}

					final tag = world.getID(c);
					res.push(
						if(ms2.length == 0 && type.isFlags())
							OInitMultiTKind(typeref, tag)
						else
							OInitTKind(typeref, tag)
					);

					for(mem in ms2) {
						final id = world.getInstID(mem);

						res.push(ODup);
						res = res.concat(compile(ctx, args[i]));
						res.push(OSetMember(id));
						i++;
					}

					res;
				},
				at(MSTaggedCase(ms1, c, ms2, null)) => {
					var res: Opcodes = [];

					var i = 0;
					final endInit = args.length - ms2.length;
					while(i < endInit) {
						res = res.concat(compile(ctx, args[i]));
						i++;
					}

					final tag = world.getID(c);
					res.push(
						if(ms2.length == 0 && type.isFlags())
							throw "Multi-kinds cannot have fields!"
						else
							OInitTKind(typeref, tag)
					);

					for(mem in ms1) {
						final id = world.getInstID(mem);

						res.push(OSwap);
						res.push(ODup2);
						res.push(OSetMember(id));
					}

					for(mem in ms2) {
						final id = world.getInstID(mem);

						res.push(ODup);
						res = res.concat(compile(ctx, args[i]));
						res.push(OSetMember(id));
						i++;
					}

					res;
				},
				at(MSTaggedCase(ms1, c, ms2, partial!!)) => {
					[];
				},

				at(MSTaggedCaseAlias(c)) => {
					[];
				},

				at(MSFromTypevar(_, _, _, _)) => {
					[];
				},
				at(MSFromParent(parent, kind)) => {
					[];
				}
			);
		},
		_ => {
			[];
		} 
	);
}

overload static function compile(ctx: GenCtx, candidates: Array<ObjMessage.ObjMultiCandidate>, args: Array<TExpr>, wantValue: Bool): Opcodes {
	return candidates._match(
		at([]) => throw "bad",
		at([{kind: kind, tctx: tctx}]) => kind._match(
			at(MIMethod(mth = {isMacro: true}, _)) => {
				if(mth.decl.isNative(NBool)) {
					mth.fuzzyName._match(
						at("yes:no:") => { // inline if-else macro method Bool#yes:no:
							[OIfElse(
								compile(ctx, args[0]),
								compile(ctx, args[1])
							)];
						},
						_ => throw "NYI"
					);
				} else {
					throw "NYI";
				}
			},
			at(MIMethod(mth, null)) => {
				mth.native._and(native => native._match(
					at(None) => throw "bad",
					at(Some({name: name})) => {
						var res = [];
						for(arg in args) {
							res = res.concat(compile(ctx, arg));
						}
						res.push(ONative(name));
						return res;
					}
				));

				final ictx = tctx._andOr(tctx => (
					if(tctx.size() > 0) {
						final _ictx = new TVarInstCtx();
						for(tv => t in tctx) {
							_ictx[world.getTVar(tv)] = world.getTypeRef(t);
						}
						_ictx;
					} else
						null
				), null);

				final id = world.getID(mth);
				var res = [];
				for(arg in args) {
					res = res.concat(compile(ctx, arg));
				}
				res.push(mth.typedBody != null ? OSend_MI(id, ictx) : OSendDynamic_MI(id, ictx));
				if(!wantValue && mth.ret._andOr(ret => ret != Pass2.STD_Void.thisType, false)) {
					res.push(OPop);
				}
				res;
			},
			at(MIMethod(mth, partial!!)) => {
				[];
			},
			at(MIMember(mem)) => {
				final id = world.getInstID(mem);
				var res: Opcodes = if(args.length == 1) compile(ctx, args[0]) else [];
				res.push(wantValue ? OTeeMember(id) : OSetMember(id));
				res;
			},
			at(MIFromTypevar(_, _, _, _)) => {
				[];
			},
			at(MIFromParent(parent, kind)) => {
				[];
			}
		),
		_ => {
			[];
		} 
	);
}

overload static function compile(target: Type, candidates: Array<CastKind>): Opcodes {
	return candidates._match(
		at([CMethod(mth, tctx)]) => {
			mth.native._and(native => native._match(
				at(None) => throw "bad",
				at(Some({name: name})) => {
					return [ONative(name)];
				}
			));

			final ictx = tctx._andOr(tctx => (
				if(tctx.size() > 0) {
					final _ictx = new TVarInstCtx();
					for(tv => t in tctx) {
						_ictx[world.getTVar(tv)] = world.getTypeRef(t);
					}
					_ictx;
				} else
					null
			), null);

			final id = world.getID(mth);
			[mth.typedBody != null ? OSend_C(id, ictx) : OSendDynamic_C(id, ictx)];
		},
		at([CUpcast(parent)]) => {
			final tref = world.getTypeRef(parent);
			[OUpcast(tref)];
		},
		at([CDowncast(child)]) => {
			final tref = world.getTypeRef(child);
			[ODowncast(tref)];
		},
		at([CNative(t)]) => {
			final tref = world.getTypeRef(t);
			[ONativeCast(tref)];
		},
		_ => {
			trace("TODO: ", candidates.map(c -> c._match(
				at(CMethod(m, _)) => m.span.display(),
				_ => Std.string(c)
			)));
			[];
		}
	);
}

overload static function compile(ctx: GenCtx, kinds: Array<TExpr.BinaryOpCandidate>, right: TExpr, wantValue: Bool): Opcodes {
	return kinds._match(
		at([{kind: kind, tctx: tctx}]) => {
			var res: Opcodes = [];
			
			final mth = kind.digForMethod();
			if(mth.isMacro) {
				if(mth.decl.thisType == Pass2.STD_Bool) {
					mth.op._match(
						at(And) => {
							res.push(OIfElse(
								compile(ctx, right),
								[OFalse]
							));
						},
						at(Or) => {
							res.push(OIfElse(
								[OTrue],
								compile(ctx, right)
							));
						},
						at(Xor) => {
							// TODO
							res = res.concat(compile(ctx, right));
							res.push(ONative("bool_ne"));
						},
						at(Nor) => {
							function loop(expr: TExpr): Opcodes return expr.e._match(
								at(EInfix(left, [{kind: _.digForMethod() => mth}], right),
								when(mth.decl.thisType == Pass2.STD_Bool && mth.op == Nor)) => {
									final res = compile(ctx, left);
									res.push(OIfElse(
										[OFalse],
										loop(right)
									));
									res;
								},
								_ => {
									final res = compile(ctx, expr);
									res.push(ONative("bool_not"));
									res;
								}
							);

							res.push(OIfElse(
								[OFalse],
								loop(right)
							));
						},
						_ => throw "NYI"
					);
				} else {
					throw "NYI";
				}
			} else {
				res = res.concat(compile(ctx, right));
				mth.native._andOr(native => {
					native._match(
						at(None) => throw "???",
						at(Some({name: name})) => {
							
							res.push(ONative(name));
							tctx._and(tctx => if(tctx.size() > 0) {
								trace("???"+tctx.display());
							});
						}
					);
				}, {
					tctx._and(tctx => if(tctx.size() > 0) throw "NYI");

					final id = world.getID(mth);
					res.push(mth.typedBody != null ? OSend_BO(id) : OSendDynamic_BO(id));
				});
			}

			if(!wantValue) {
				res.push(OPop);
			}

			res;
		},
		_ => throw "NYI"
	);
}

static function compilePrefix(kind: UnaryOpKind, wantValue: Bool) {
	var res: Opcodes = [];

	final mth = kind.digForMethod();

	mth.native._andOr(native => {
		native._match(
			at(None) => throw "???",
			at(Some({name: name})) => {
				res.push(ONative(name));
			}
		);
	}, {
		final id = world.getID(mth);
		res.push(mth.typedBody != null ? OSend_UO(id) : OSendDynamic_UO(id));
	});

	if(!wantValue) {
		res.push(OPop);
	}

	return res;
}

static function compileSuffix(kind: UnaryOpKind, wantValue: Bool) {
	var res: Opcodes = [];

	final mth = kind.digForMethod();

	final isStep = mth.op._match(at(Incr | Decr) => true, _ => false);
	if(wantValue && isStep) {
		res.push(ODup);
	}

	mth.native._andOr(native => {
		native._match(
			at(None) => throw "???",
			at(Some({name: name})) => {
				res.push(ONative(name));
			}
		);
	}, {
		final id = world.getID(mth);
		res.push(mth.typedBody != null ? OSend_UO(id) : OSendDynamic_UO(id));
	});

	if(!wantValue || isStep) {
		res.push(OPop);
	}

	return res;
}


}