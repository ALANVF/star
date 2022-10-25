package codegen;

import typing.*;
import Util.detuple;
import codegen.Opcode;

private enum Capture {
	CLocal(id: LocalID, t: TypeRef);
	CAssign(id: LocalID, t: TypeRef, assignTo: TExpr);
}

@:publicFields
@:build(util.Overload.build())
class CodeGen {
//

static final world = new World2();


static function compileProgram(project: Project) {
	if(project.useStdlib && project != Project.STDLIB) {
		compileProgram(Project.STDLIB);
	}

	// TODO: improve, maybe use visitor pattern
	for(file in project.allFiles()) {
		for(decl in file.sortedDecls) if(!(decl is DirectAlias)) world.add(decl);
		for(cat in file.categories) world.add(cat);
	}
}


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
				at(PMy(name)) => ctx.newLocal(name),
				at(PIgnore) => ctx.anon(),
				_ => throw "NYI"
			), {
				ctx.anon();
			});
			
			res.push(ODup);
			res.push(ONewLocal);
			res.push(OGetMember(world.getInstID(length.getMember())));
			res.push(OSetLocal(lenName));

			res.push(ONewLocal);
			res.push(OGetMember(world.getInstID(buffer.getMember())));
			res.push(OSetLocal(bufName));

			res.push(ONewLocal);
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
				etype.iterAssocType()._andOr(assocType => {
					detuple(@final [keyType, valueType] = assocType.nonNull()); // TODO: cache this or smth
					final iterType = Pass2.STD_Iterator2.applyArgs([keyType, valueType]).nonNull();

					var overloads = CastKind.reduceOverloads(
						etype.findCast(/*BAD*/{where:WBlock,thisType:etype}, iterType, etype),
						etype,
						iterType
					);

					// hacky
					if(overloads.every(ov -> ov._match(
						at(CMethod({typedBody: null})) => true,
						_ => false
					))) {
						overloads = [overloads[0]];
					}

					final casts = overloads/*.filter(
						k -> k._match(
							at(CMethod(m)) => (etype.t.match(TTypeVar(_) | TInstance(_ == Pass2.STD_Iterable2 => true, _, _))),
							_ => true
						)
					)*/._match(
						at([]) => throw 'Cannot iterate on type `${etype.fullName()}`!',
						at(kinds = [_]) => kinds,
						at(kinds) => throw 'Cannot find specific iterator for type `${etype.fullName()}`!'+kinds.map(
							k -> k._match(
								at(CMethod(m)) => m.span.display(),
								_ => throw "bad"
							)
						)
					);

					final iterNext = iterType.findSingleInst(/*BAD*/null, "next", Pass2.STD_Iterator2).nonNull();
					final iterRet = iterNext.retType().nonNull();
					final noneCase = iterRet.allTaggedCases().find(c -> c is SingleTaggedCase).nonNull();
					final noneCaseID = world.getID(noneCase);

					res = res.concat(compile(etype, iterType, casts));
					res.push(ONewLocal);
					res.push(OSetLocal(iterName));

					final ctx2 = ctx.inner();
					var loop = [
						OGetLocal(iterName)
					];

					loop = loop.concat(compile(ctx2, iterType, iterNext, true));
					
					loop.push(ODup);
					loop.push(OKindID);
					loop.push(OTCaseID(world.getTypeRef(iterRet), noneCaseID));
					loop.push(ONative("caseid_eq"));
					loop.push(OIf([
						OBreak(label2)
					]));

					loop.push(OKindSlot(0));

					final captures = new Map<String, Capture>();
					final pat1 = compile(ctx2, exitLoop, captures, keyType, kpat, true, true, true);
					final pat2 = compile(ctx2, exitLoop, captures, valueType, vpat, true, true, true);

					
					compileCapturesPre(ctx2, loop, captures);
					loop.push(ODup);
					loop.push(OGetMember(1));
					loop = loop.concat(pat1);
					loop.push(OGetMember(2));
					loop = loop.concat(pat2);
					cond._and(cond => {
						loop = loop.concat(compile(ctx2, cond));
						loop.push(exitLoop);
					});
					compileCapturesPost(ctx2, loop, captures);

					loop = loop.concat(compile(ctx2, body));

					res.push(OLoop(label2, loop));
				}, {
					// TODO: don't duplicate code
					final elemType = etype.iterElemType().nonNull(); // TODO: cache this or smth
					final iterType = Pass2.STD_Iterator1.applyArgs([elemType]).nonNull();
					
					var overloads = CastKind.reduceOverloads(
						etype.findCast(/*BAD*/{where:WBlock,thisType:etype}, iterType, etype),
						etype,
						iterType
					);

					// hacky
					if(overloads.every(ov -> ov._match(
						at(CMethod({typedBody: null})) => true,
						_ => false
					))) {
						overloads = [overloads[0]];
					}

					// hacky
					if(overloads.some(k -> k._match(
						at(CMethod(m)) => (m.decl == Pass2.STD_Iterable1),
						_ => false
					)) && overloads.some(k -> k._match(
						at(CMethod(m)) => (m.decl != Pass2.STD_Iterable1),
						_ => true
					))) {
						overloads = overloads.filter(k -> k._match(
							at(CMethod(m)) => (m.decl != Pass2.STD_Iterable1),
							_ => true
						));
					}

					final casts = overloads/*.filter(
						k -> k._match(
							at(CMethod(m)) => (etype.t.match(TTypeVar(_) | TInstance(_ == Pass2.STD_Iterable1 => true, _, _))),
							_ => true
						)
					)*/._match(
						at([]) => throw 'Cannot iterate on type `${etype.fullName()}`!',
						at(kinds = [_]) => kinds,
						at(kinds) => throw 'Cannot find specific iterator for type `${etype.fullName()}`!'+kinds.map(
							k -> k._match(
								at(CMethod(m)) => m.span.display(),
								_ => throw "bad"
							)
						)
					);

					final iterNext = iterType.findSingleInst(/*BAD*/null, "next", Pass2.STD_Iterator1).nonNull();
					final iterRet = iterNext.retType().nonNull();
					final noneCase = iterRet.allTaggedCases().find(c -> c is SingleTaggedCase).nonNull();
					final noneCaseID = world.getID(noneCase);

					final iname = kpat._andOr(kp => kp.p._match(
						at(PMy(name)) => ctx.newLocal(name),
						at(PIgnore) => ctx.anon(),
						_ => throw "NYI"
					), {
						ctx.anon();
					});

					res = res.concat(compile(etype, iterType, casts));
					res.push(ONewLocal);
					res.push(OSetLocal(iterName));

					res.push(ONewLocal);
					res.push(OInt32(0, true));
					res.push(OSetLocal(iname));

					final ctx2 = ctx.inner();
					var loop = [
						OGetLocal(iterName)
					];

					loop = loop.concat(compile(ctx2, iterType, iterNext, true));
					
					loop.push(ODup);
					loop.push(OKindID);
					loop.push(OTCaseID(world.getTypeRef(iterRet), noneCaseID));
					loop.push(ONative("caseid_eq"));
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

					res.push(OLoopThen(label2, loop, [
						OGetLocal(iname),
						ONative("i32_succ"),
						OSetLocal(iname)
					]));
				});
			}, {
				final elemType = etype.iterElemType().nonNull(); // TODO: cache this or smth
				final iterType = Pass2.STD_Iterator1.applyArgs([elemType]).nonNull();
				
				var overloads = CastKind.reduceOverloads(
					etype.findCast(/*BAD*/{where:WBlock,thisType:etype}, iterType, etype),
					etype,
					iterType
				);

				// hacky
				if(overloads.every(ov -> ov._match(
					at(CMethod({typedBody: null})) => true,
					_ => false
				))) {
					overloads = [overloads[0]];
				}

				// hacky
				if(overloads.some(k -> k._match(
					at(CMethod(m)) => (m.decl == Pass2.STD_Iterable1),
					_ => false
				)) && overloads.some(k -> k._match(
					at(CMethod(m)) => (m.decl != Pass2.STD_Iterable1),
					_ => true
				))) {
					overloads = overloads.filter(k -> k._match(
						at(CMethod(m)) => (m.decl != Pass2.STD_Iterable1),
						_ => true
					));
				}

				final casts = overloads/*.filter(
					k -> k._match(
						at(CMethod(m)) => (etype.t.match(TTypeVar(_) | TInstance(_ == Pass2.STD_Iterable1 => true, _, _))),
						_ => true
					)
				)*/._match(
					at([]) => throw 'Cannot iterate on type `${etype.fullName()}`!',
					at(kinds = [_]) => kinds,
					at(kinds) => throw 'Cannot find specific iterator for type `${etype.fullName()}`!'+kinds.map(
						k -> k._match(
							at(CMethod(m)) => m.span.display(),
							_ => throw "bad"
						)
					)
				);

				final iterNext = iterType.findSingleInst(/*BAD*/null, "next", Pass2.STD_Iterator1).nonNull();
				final iterRet = iterNext.retType().nonNull();
				final noneCase = iterRet.allTaggedCases().find(c -> c is SingleTaggedCase).nonNull();
				final noneCaseID = world.getID(noneCase);

				res = res.concat(compile(etype, iterType, casts));
				res.push(ONewLocal);
				res.push(OSetLocal(iterName));

				final ctx2 = ctx.inner();
				var loop = [
					OGetLocal(iterName)
				];

				loop = loop.concat(compile(ctx2, iterType, iterNext, true));
				
				loop.push(ODup);
				loop.push(OKindID);
				loop.push(OTCaseID(world.getTypeRef(iterRet), noneCaseID));
				loop.push(ONative("caseid_eq"));
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
			at(EVarDecl(name, _, _)) => ctx.newLocal(name),
			_ => throw "NYI"
		), {
			ctx.anon();
		});

		detuple(@final [startKind, startExpr] = start);
		detuple(@final [stopKind, stopExpr] = stop);

		final startType: Type = startExpr.t ?? throw "bad";
		final stopType: Type = stopExpr.t ?? throw "bad";
		if(startType != stopType) throw "todo";

		res.push(ONewLocal);

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
					
					res.push(ONewLocal);
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
				//final byType: Type = e.t ?? throw "bad";

				res.push(ONewLocal);
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
				ONative(downwards? "i32_pred" : "i32_succ"),
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

	at(SBreak(null)) => [OBreak(ctx.getLoop() ?? throw "Cannot break here!")],
	at(SBreak(Left(depth))) => [OBreak(ctx.getLoop(depth) ?? throw "Cannot break here!")],
	at(SBreak(Right(label))) => [OBreak(ctx.label(label))],

	at(SNext(null)) => [ONext(ctx.getLoop() ?? throw "Cannot continue here!")],
	at(SNext(Left(depth))) => [ONext(ctx.getLoop(depth) ?? throw "Cannot continue here!")],
	at(SNext(Right(label))) => [ONext(ctx.label(label))],

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
					[mem.isStatic ? OGetStaticField(world.getStaticID(mem)) : OGetField(world.getInstID(mem))];
				},
				at(tuple(thisName, thisType)) => {
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
			[OGetLocal(ctx.local(name))];
		}
	);
}

static function setName(ctx: GenCtx, name: String, loc: Local): Opcode {
	return loc._match(
		at(lf is Pass2.LocalField) => {
			final mem = lf.member;
			mem.isStatic ? OSetStaticField(world.getStaticID(mem)) : OSetField(world.getInstID(mem));
		},
		_ => {
			// TODO
			OSetLocal(ctx.local(name));
		}
	);
}

static function setOrTeeName(ctx: GenCtx, name: String, loc: Local, wantValue: Bool): Opcode {
	return loc._match(
		at(lf is Pass2.LocalField) => {
			final mem = lf.member;
			mem.isStatic
				? (wantValue ? OTeeStaticField(world.getStaticID(mem)) : OSetStaticField(world.getStaticID(mem)))
				: (wantValue ? OTeeField(world.getInstID(mem)) : OSetField(world.getInstID(mem)));
		},
		_ => {
			// TODO
			wantValue ? OTeeLocal(ctx.local(name)) : OSetLocal(ctx.local(name));
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
		// TODO: merge with `Type <char>` compile logic
		[OChar(char)];
	} else [],

	at(EStr([])) => if(wantValue) [OStr("")] else [],
	at(EStr([PStr(str)])) => if(wantValue) [OStr(str)] else [],
	at(EStr(parts)) => {
		var offset: Int;
		var res: Opcodes = parts[0]._match(
			at(PStr(str)) => {
				offset = 1;
				[OStr(str)];
			},
			at(PCode(_)) => {
				offset = 0;
				[OStr("")];
			}
		);

		for(i in offset...parts.length) {
			parts[i]._match(
				at(p = PStr(str)) => {
					final estr: TExpr = {e: EStr([p]), orig: null, t: Pass2.STD_Str};
					final overloads = BinaryOpKind.reduceOverloads(
						Pass2.STD_Str.findBinaryOp(null, Plus, Pass2.STD_Str),
						Pass2.STD_Str,
						estr
					);
					res = res.concat(compile(ctx, Pass2.STD_Str, overloads, estr, true));
				},
				at(PCode(expr)) => {
					final overloads = BinaryOpKind.reduceOverloads(
						Pass2.STD_Str.findBinaryOp(null, Plus, Pass2.STD_Str),
						Pass2.STD_Str,
						expr
					);
					res = res.concat(compile(ctx, Pass2.STD_Str, overloads, expr, true));
				}
			);
		}

		res;
	},

	at(EBool(true)) => if(wantValue) [OTrue] else [],
	at(EBool(false)) => if(wantValue) [OFalse] else [],
	
	at(EArray(values)) => {
		final type = expr.t.nonNull();
		if(type.t.match(TMulti(_))) throw "type inference is not currently available for arrays! "+expr.orig.mainSpan().display();
		var res: Opcodes;
		do {
			if(values.length > 0) {
				final sizeExpr: TExpr = {
					e: EInt(values.length, null),
					t: Pass2.STD_Int
				};

				MultiStaticKind.reduceOverloads(
					type.findMultiStatic(/*BAD*/null, ["new"], type),
					type,
					[sizeExpr]
				)._match(
					at([]) => {},
					at(overloads) => {
						res = compile(ctx, type, type, overloads, [sizeExpr], true);
						break;
					}
				);
			}
			
			final kind = type.findSingleStatic(/*BAD*/null, "new", type).nonNull();
			res = compile(ctx, type, type, kind, true);
		} while(false);

		for(value in values) {
			final overloads = MultiInstKind.reduceOverloads(
				type.findMultiInst(/*BAD*/null, ["add"], type),
				/*BAD*/null,
				type,
				[value]
			);

			res.push(ODup);
			res = res.concat(compile(ctx, type, overloads, [value], false, false));
		}
		
		res;
	},

	at(EHash(pairs)) => {
		final type = expr.t.nonNull();
		//if(type.t.match(TMulti(_))) throw "type inference is not currently available for dictionaries!";
		var res: Opcodes;
		do {
			if(pairs.length > 0) {
				final sizeExpr: TExpr = {
					e: EInt(pairs.length, null),
					t: Pass2.STD_Int
				};

				MultiStaticKind.reduceOverloads(
					type.findMultiStatic(/*BAD*/null, ["new"], type),
					type,
					[sizeExpr]
				)._match(
					at([]) => {},
					at(overloads) => {
						res = compile(ctx, type, type, overloads, [sizeExpr], true);
						break;
					}
				);
			}
			
			final kind = type.findSingleStatic(/*BAD*/null, "new", type).nonNull();
			res = compile(ctx, type, type, kind, true);
		} while(false);

		for(pair in pairs) { detuple(@final [key, value] = pair);
			final overloads = MultiInstKind.reduceOverloads(
				type.findMultiInst(/*BAD*/null, ["atNew", "set"], type),
				/*BAD*/null,
				type,
				[key, value]
			);

			res.push(ODup);
			res = res.concat(compile(ctx, type, overloads, [key, value], false, false));
		}
		
		res;
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

		for(i => mem in members) {
			res.push(ODup);
			res = res.concat(compile(ctx, values[i]));
			res.push(OSetMember(world.getInstID(mem)));
		}

		res;
	},

	at(EThis) => if(wantValue) {
		[ctx.getThis()._match(
			at(null) => OThis,
			at(tuple(name, type)) => OGetLocal(name)
		)];
	} else [],

	at(EWildcard) => throw "bad",

	at(EFunc(params, ret, body)) => {
		trace("TODO "+expr.orig.mainSpan().display());
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
				at(NFloat32) => OFloat32(int, "0", exp),
				at(NFloat64) => OFloat64(int, "0", exp),
				at(NDec64) => ODec64(int, "0", exp),
				_ => throw "Cannot construct int literal from non-numeric type!"
			)
		)];
	},
	// TODO: make this less ugly
	at(ELiteralCtor(type, {e: EPrefix(UOMethod({op: Neg, native: Some({name: _.endsWith("_neg") => true})}), {e: EInt(int, exp)})})) => {
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
				at(NBool) => throw "Cannot construct bool literal using a negative number!",
				at(NInt8) => OInt8(-intVal(), kind == NInt8),
				at(NInt16) => OInt16(-intVal(), kind == NInt16),
				at(NInt32) => OInt32(-intVal(), kind == NInt32),
				at(NInt64) => {
					OInt64(exp._andOr(
						e => haxe.Int64.parseString('-${int}e$e'), // TODO: improve
						-haxe.Int64.ofInt(int)
					), kind == NInt64);
				},
				at(NUInt8 | NUInt16 | NUInt32 | NUInt64) => throw "Cannot construct unsigned int literal using a negative number!",
				at(NFloat32) => OFloat32(-int, "0", exp),
				at(NFloat64) => OFloat64(-int, "0", exp),
				at(NDec64) => ODec64(-int, "0", exp),
				_ => throw "Cannot construct int literal from non-numeric type!"
			)
		)];
	},
	at(ELiteralCtor(type, {e: EDec(int, dec, exp)})) => {
		[type.getNative()._match(
			at(null) => throw "Cannot construct dec literal from non-native type!",
			at(kind!!) => kind._match(
				at(NFloat32) => OFloat32(int, dec, exp),
				at(NFloat64) => OFloat64(int, dec, exp),
				at(NDec64) => ODec64(int, dec, exp),
				_ => throw "Cannot construct dec literal from non-numeric type!"
			)
		)];
	},
	at(ELiteralCtor(type, {e: EPrefix(UOMethod({op: Neg, native: Some({name: _.endsWith("_neg") => true})}), {e: EDec(int, dec, exp)})})) => {
		[type.getNative()._match(
			at(null) => throw "Cannot construct dec literal from non-native type!",
			at(kind!!) => kind._match(
				at(NFloat32) => OFloat32(-int, dec, exp),
				at(NFloat64) => OFloat64(-int, dec, exp),
				at(NDec64) => ODec64(-int, dec, exp),
				_ => throw "Cannot construct dec literal from non-numeric type!"
			)
		)];
	},
	at(ELiteralCtor(type, {e: EArray(values)})) => {
		var res: Opcodes;
		do {
			if(values.length > 0) {
				final sizeExpr: TExpr = {
					e: EInt(values.length, null),
					t: Pass2.STD_Int
				};

				MultiStaticKind.reduceOverloads(
					type.findMultiStatic(/*BAD*/null, ["new"], type),
					type,
					[sizeExpr]
				)._match(
					at([]) => {},
					at(overloads) => {
						res = compile(ctx, type, type, overloads, [sizeExpr], true);
						break;
					}
				);
			}
			
			final kind = type.findSingleStatic(/*BAD*/null, "new", type).nonNull();
			res = compile(ctx, type, type, kind, true);
		} while(false);

		for(value in values) {
			final overloads = MultiInstKind.reduceOverloads(
				type.findMultiInst(/*BAD*/null, ["add"], type),
				/*BAD*/null,
				type,
				[value]
			);

			res.push(ODup);
			res = res.concat(compile(ctx, type, overloads, [value], false, false));
		}
		
		res;
	},
	at(ELiteralCtor(type, {e: EHash(pairs)})) => {
		var res: Opcodes;
		do {
			if(pairs.length > 0) {
				final sizeExpr: TExpr = {
					e: EInt(pairs.length, null),
					t: Pass2.STD_Int
				};

				MultiStaticKind.reduceOverloads(
					type.findMultiStatic(/*BAD*/null, ["new"], type),
					type,
					[sizeExpr]
				)._match(
					at([]) => {},
					at(overloads) => {
						res = compile(ctx, type, type, overloads, [sizeExpr], true);
						break;
					}
				);
			}
			
			final kind = type.findSingleStatic(/*BAD*/null, "new", type).nonNull();
			res = compile(ctx, type, type, kind, true);
		} while(false);

		for(pair in pairs) { detuple(@final [key, value] = pair);
			final overloads = MultiInstKind.reduceOverloads(
				// because #atNew:set: is really just there for Dict
				type.findMultiInst(/*BAD*/null, ["atNew", "set"], type)._match(
					at([]) => type.findMultiInst(/*BAD*/null, ["at", "set"], type),
					at(ovs) => ovs
				),
				/*BAD*/null,
				type,
				[key, value]
			);

			res.push(ODup);
			res = res.concat(compile(ctx, type, overloads, [key, value], false, false));
		}
		
		res;
	},
	at(ELiteralCtor(type, literal)) => {
		trace("TODO "+expr.orig.mainSpan().display());
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

	at(ETypeMessage(type, msg)) => compile(ctx, expr.t.nonNull(), type, msg, wantValue),

	at(ETypeCascade(type, cascades)) => {
		var res: Opcodes = [];

		for(cascade in cascades) {
			final wantValue = cascade.nested.length > 0;
			cascade.kind._match(
				at(Lazy(_)) => throw "bad",
				at(Member(msg)) => {
					res.pushAll(compile(ctx, cascade.t.nonNull(), type, msg, wantValue));
				},
				at(Message(msg)) => {
					res.pushAll(compile(ctx, cascade.t.nonNull(), type, msg, wantValue));
				},
				at(AssignMember(setMem, op, expr)) => {
					if(op != null) throw "todo";
					res.pushAll(compile(ctx, cascade.t.nonNull(), type, setMem, wantValue));
				},
				at(AssignMessage(setMsg, op, expr)) => {
					if(op != null) throw "todo";
					res.pushAll(compile(ctx, cascade.t.nonNull(), type, setMsg, wantValue));
				},
				at(StepMember(setMem, getMem, step)) => {
					res.push(ODup);
					final memType = cascade.t.nonNull();
					res.pushAll(compile(ctx, memType, type, getMem, true));
					res.pushAll(compilePrefix(memType, step, true));
					res.pushAll(compile(ctx, memType, type, [{kind: setMem, tctx: null}], [], wantValue));
				},
				at(StepMessage(setMsg, getMsg, step)) => {
					throw "todo";
				},
				at(Block(_, blk)) => {
					// TODO
					res.push(OBlock(
						compile(ctx.addThis(0 /*?*/, world.getTypeRef(type)), blk)
					));
				}
			);

			if(wantValue) {
				final type2 = cascade.t.nonNull();

				for(c in cascade.nested) {
					res.push(ODup);
					compile(ctx, res, type2, c);
				}

				res.push(OPop);
			}
		}

		res;
	},

	at(ETypeMember(type, kind)) => compile(ctx, expr.t.nonNull(), type, kind, wantValue),

	at(EObjMessage(expr, msg)) => compile(ctx, expr).concat(compile(ctx, expr.t.nonNull(), msg, wantValue)),

	at(EObjCascade(expr, cascades)) => {
		final type = expr.t.nonNull();
		final res = compile(ctx, expr);

		for(cascade in cascades) {
			res.push(ODup);
			compile(ctx, res, type, cascade);
		}

		if(!wantValue) {
			res.push(OPop);
		}

		res;
	},

	at(EObjMember(expr, kind)) => compile(ctx, expr).concat(compile(ctx, expr.t.nonNull(), kind, wantValue)),

	at(EObjLazyMember(_, _)) => throw "this should not be here!",

	at(EPrefix(kind, right)) => compile(ctx, right).concat(compilePrefix(right.t.nonNull(), kind, wantValue)),

	at(ELazyPrefix(_, _)) => throw "this should not be here!",

	at(ESuffix(left, kind)) => compile(ctx, left).concat(compileSuffix(left.t.nonNull(), kind, wantValue)),

	at(ELazySuffix(_, _)) => throw "this should not be here!",

	at(EInfix(left, kinds, right)) => compile(ctx, left).concat(compile(ctx, left.t.nonNull(), kinds, right, wantValue)),

	at(ELazyInfix(_, _, _)) => throw "this should not be here!",

	at(EInfixChain(left, chain)) => throw "todo",

	at(EVarDecl(name, _, value)) => {
		final res: Opcodes = [ONewLocal];
		final id = ctx.newLocal(name);
		value._andOr(v => {
			res
			.concat(compile(ctx, v))
			.concat([
				wantValue ? OTeeLocal(id) : OSetLocal(id)
			]);
		}, {
			res;
		});
	},

	at(ESetName(name, loc, value)) => {
		final isStep = value.e._match(
			at(ESuffix({e: EName(_ == name => true, _)}, kind)) => kind.isStep(),
			_ => false
		);
		
		final res = compile(ctx, value);
		res.push(setOrTeeName(ctx, name, loc, !isStep));
		res;
	},


	at(EDestructure(pattern, value)) => {
		if(wantValue) throw "absolutely not";

		pattern.p._match(
			at(PTuple(patterns)) => {
				var res: Opcodes = [];
				
				value.e._match(
					at(ETuple(values)) => {
						if(patterns.length != values.length) throw "bad";
						for(i => v in values) {
							patterns[i].p._match(
								at(PIgnore) => continue,
								_ => {
									res = res.concat(
										compile(ctx, v)
									);
								}
							);
						}
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

						for(i => p in patterns) { final mem = members[i];
							p.p._match(
								at(PIgnore) => continue,
								_ => {
									final memID = world.getInstID(mem);
									res.push(ODup);
									res.push(OGetMember(memID));
								}
							);
						}
					}
				);

				final lenp = patterns.length;
				for(i in 0...lenp) { final pat = patterns[lenp - i - 1]; pat.p._match(
					at(PExpr(expr)) => expr.e._match(
						at(EName(name, loc)) => {
							res.push(setName(ctx, name, loc));
						},

						at(ELiteralCtor(type, literal)) => throw "NYI",
						at(ETypeMessage(type, msg)) => throw "NYI",
						at(ETypeMember(type, kind)) => throw "NYI",
						at(EObjCascade(expr, cascades)) => throw "NYI",
						at(EObjMember(obj, kind)) => throw "NYI",
						
						at(EVarDecl(name, _, null)) => {
							res.push(ONewLocal);
							res.push(OSetLocal(ctx.newLocal(name)));
						},

						_ => throw "bad"
					),

					at(PExtractor(_)) => throw "???",

					at(PIgnore) => continue,

					at(PMy(name)) => {
						res.push(ONewLocal);
						res.push(OSetLocal(ctx.newLocal(name)));
					},

					at(PMyType(name, type)) => {
						// TODO
						final typeref = world.getTypeRef(type.nonNull());
						
						res.push(ONewLocal);
						res.push(ODup);
						res.push(OOfType(typeref));
						res.push(OIfNot([
							OStr("Match error!"), // TODO
							OThrow(pat.orig?.mainSpan().display() ?? "???")
						]));
						res.push(ONativeCast(typeref)); // TODO?
						res.push(OSetLocal(ctx.newLocal(name)));
					},

					_ => throw "NYI"
				); }

				res;
			},
			_ => {
				trace("TODO");
				[];
			}
		);
	},

	at(EInitThis(type, msg)) => {
		final res = compile(ctx, type, type, msg, true);
		res.setLast(res.last()._match(
			at(OSend_IS(t, init)) => OInitThis_S(t, init),
			at(OSend_IM(t, init, ctx)) => OInitThis_M(t, init, ctx),
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
			
			final res = compile(ctx, target, eq, expr, true);
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

			final res = compile(ctx, target, msg, true);
			if(!isRhs) {
				res.push(exitCase);
			}
			return res;
		},

		at(PIgnore) => {
			return if(isRhs) [] else [OPop];
		},

		at(PMy(name)) => {
			final id = ctx.newLocal(name);
			captures[name]._match(
				at(null) => {
					captures[name] = CLocal(id, world.getTypeRef(target));
					return [OSetLocal(id)];
				},
				at(CLocal(_, type) | CAssign(_, type, _)) => {
					if(uniqueCaptures) throw "bad";
					if(!type.equals(world.getTypeRef(target))) throw "bad";
					return [OSetLocal(id)];
				}
			);
		},

		at(PMyType(name, type)) => {
			final typeref = world.getTypeRef(type);
			//final targetref = world.getTypeRef(target);
			final id = ctx.newLocal(name);
			captures[name]._match(
				at(null) => {
					captures[name] = CLocal(id, typeref);
					return [
						ODup,
						OOfType(typeref),
						exitCase,
						if(type.hasParentType(target)) ODowncast(typeref) else OUpcast(typeref),
						OSetLocal(id)
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
						OSetLocal(id)
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
					
					res = res.concat(compile(ctx, target, cmp, expr, true));
					res.push(exitCase);
				},
				at(PTypeValueCase(type, valueCase)) => {
					res.push(OKindID);
					res.push(OVCaseID(world.getTypeRef(type), world.getID(valueCase)));
					res.push(ONative(bound._match(
						at(Inclusive) => "caseid_ge",
						at(Exclusive) => "caseid_gt"
					)));
					res.push(exitCase);
				},
				at(PTypeTaggedCaseSingle(type, taggedCase)) => {
					res.push(OKindID);
					res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
					res.push(ONative(bound._match(
						at(Inclusive) => "caseid_ge",
						at(Exclusive) => "caseid_gt"
					)));
					res.push(exitCase);
				},
				at(PTypeTaggedCaseMulti(type, taggedCase, args)) => {
					if(!args.every(a -> a.p.match(PIgnore))) throw "bad";
					
					res.push(OKindID);
					res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
					res.push(ONative(bound._match(
						at(Inclusive) => "caseid_ge",
						at(Exclusive) => "caseid_gt"
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
					
					res = res.concat(compile(ctx, target, cmp, expr, true));
					res.push(exitCase);
				},
				at(PTypeValueCase(type, valueCase)) => {
					res.push(OKindID);
					res.push(OVCaseID(world.getTypeRef(type), world.getID(valueCase)));
					res.push(ONative(bound._match(
						at(Inclusive) => "caseid_le",
						at(Exclusive) => "caseid_lt"
					)));
					res.push(exitCase);
				},
				at(PTypeTaggedCaseSingle(type, taggedCase)) => {
					res.push(OKindID);
					res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
					res.push(ONative(bound._match(
						at(Inclusive) => "caseid_le",
						at(Exclusive) => "caseid_lt"
					)));
					res.push(exitCase);
				},
				at(PTypeTaggedCaseMulti(type, taggedCase, args)) => {
					if(!args.every(a -> a.p.match(PIgnore))) throw "bad";
					
					res.push(OKindID);
					res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
					res.push(ONative(bound._match(
						at(Inclusive) => "caseid_le",
						at(Exclusive) => "caseid_lt"
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
					
					res = res.concat(compile(ctx, target, cmp, expr, true));
					res.push(exitCase);
				},
				at(PTypeValueCase(type, valueCase)) => {
					res.push(OKindID);
					res.push(OVCaseID(world.getTypeRef(type), world.getID(valueCase)));
					res.push(ONative(minBound._match(
						at(Inclusive) => "caseid_ge",
						at(Exclusive) => "caseid_gt"
					)));
					res.push(exitCase);
				},
				at(PTypeTaggedCaseSingle(type, taggedCase)) => {
					res.push(OKindID);
					res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
					res.push(ONative(minBound._match(
						at(Inclusive) => "caseid_ge",
						at(Exclusive) => "caseid_gt"
					)));
					res.push(exitCase);
				},
				at(PTypeTaggedCaseMulti(type, taggedCase, args)) => {
					if(!args.every(a -> a.p.match(PIgnore))) throw "bad";
					
					res.push(OKindID);
					res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
					res.push(ONative(minBound._match(
						at(Inclusive) => "caseid_ge",
						at(Exclusive) => "caseid_gt"
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
					
					res = res.concat(compile(ctx, target, cmp, expr, true));
					res.push(exitCase);
				},
				at(PTypeValueCase(type, valueCase)) => {
					res.push(OKindID);
					res.push(OVCaseID(world.getTypeRef(type), world.getID(valueCase)));
					res.push(ONative(maxBound._match(
						at(Inclusive) => "caseid_le",
						at(Exclusive) => "caseid_lt"
					)));
					res.push(exitCase);
				},
				at(PTypeTaggedCaseSingle(type, taggedCase)) => {
					res.push(OKindID);
					res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
					res.push(ONative(maxBound._match(
						at(Inclusive) => "caseid_le",
						at(Exclusive) => "caseid_lt"
					)));
					res.push(exitCase);
				},
				at(PTypeTaggedCaseMulti(type, taggedCase, args)) => {
					if(!args.every(a -> a.p.match(PIgnore))) throw "bad";
					
					res.push(OKindID);
					res.push(OTCaseID(world.getTypeRef(type), world.getID(taggedCase)));
					res.push(ONative(maxBound._match(
						at(Inclusive) => "caseid_le",
						at(Exclusive) => "caseid_lt"
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

			for(i => pat in patterns) { final mem = members[i];
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
			}

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
			res.push(ONative("caseid_eq"));
			res.push(exitCase);

			return res;
		},

		at(PTypeTaggedCaseSingle(type, taggedCase)) => {
			var res: Opcodes = if(isRhs) [ODup] else [];
			
			res.push(OTCaseID(world.getTypeRef(target.strictUnifyWithType(type)), world.getID(taggedCase)));
			res.push(ONative("caseid_eq"));
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
			res.push(ONative("caseid_eq"));
			res.push(exitCase);

			if(hasPatterns) for(i => arg in args) if(!arg.p.match(PIgnore)) {
				final argType = taggedCase.params[i].type.getFrom(type);

				res.push(ODup);
				res.push(OKindSlot(i));
				res = res.concat(
					compile(ctx, exitCase, captures, argType, arg, false, true, true)
				);
			}

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
			
			// TODO
			captures['`$anon'] = CAssign(anon, rhsref, expr);

			return [OSetLocal(anon)];
		},

		at(PExtractor(_)) => throw "???",

		at(PIgnore) => return [OPop],

		at(PMy(name)) => {
			if(captures.exists(name)) throw "bad";

			final id = ctx.newLocal(name);
			captures[name] = CLocal(id, rhsref);

			return [OSetLocal(id)];
		},

		at(PMyType(name, type)) => {
			// TODO
			final typeref = world.getTypeRef(type.nonNull());

			if(captures.exists(name)) throw "bad";

			final id = ctx.newLocal(name);
			captures[name] = CLocal(id, typeref);
			
			return [
				ODup,
				OOfType(typeref),
				exitCase,
				if(type.hasParentType(rhs)) ODowncast(typeref) else OUpcast(typeref),
				OSetLocal(id)
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

			for(i => p in patterns) { final mem = members[i];
				p.p._match(
					at(PIgnore) => continue,
					_ => {
						final memID = world.getInstID(mem);
						res.push(ODup);
						res.push(OGetMember(memID));
					}
				);
			}

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
		at(CLocal(id, t) | CAssign(id, t, _)) => {
			res.push(ONewLocal);
		}
	);
}
static function compileCapturesPost(ctx: GenCtx, res: Opcodes, captures: Map<String, Capture>) {
	if(captures.size() == 0) return;
	
	for(capture in captures) capture._match(
		at(CLocal(_, _)) => {},
		at(CAssign(id, t, expr)) => {
			res.push(OGetLocal(id));
			expr.e._match(
				at(EName(name, loc)) => {
					res.push(setName(ctx, name, loc));
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

overload static function compile(ctx: GenCtx, res: Opcodes, sender: Type, cascade: ObjCascade) {
	final wantValue = cascade.nested.length > 0;
	cascade.kind._match(
		at(Lazy(_)) => throw "bad",
		at(Member(msg)) => {
			res.pushAll(compile(ctx, sender, msg, wantValue));
		},
		at(Message(msg)) => {
			res.pushAll(compile(ctx, sender, msg, wantValue));
		},
		at(AssignMember(setMem, op, expr)) => {
			if(op != null) throw "todo";
			res.pushAll(compile(ctx, sender, setMem, wantValue));
		},
		at(AssignMessage(setMsg, op, expr)) => {
			if(op != null) throw "todo";
			res.pushAll(compile(ctx, sender, setMsg, wantValue));
		},
		at(StepMember(setMem, getMem, step)) => {
			res.push(ODup);
			res.pushAll(compile(ctx, sender, getMem, true));
			res.pushAll(compilePrefix(getMem.retType().nonNull(), step, true));
			res.pushAll(compile(ctx, sender, [{kind: setMem, tctx: null}], [], /*TODO*/false, wantValue));
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
					ONewLocal,
					OSetLocal(anon)
				].concat(
					compile(ctx.addThis(anon, typeref), blk)
				)
			));
		}
	);

	if(wantValue) {
		final ct = cascade.t.nonNull();

		for(c in cascade.nested) {
			res.push(ODup);
			compile(ctx, res, ct, c);
		}

		res.push(OPop);
	}
}

overload static function compile(ctx: GenCtx, resType: Type, type: Type, msg: TypeMessage, wantValue: Bool) return msg._match(
	at(Single(kind)) => compile(ctx, resType, type, kind, wantValue),
	at(Multi(candidates, labels, args)) => compile(ctx, resType, type, candidates, args, wantValue),
	at(Super(parent, msg)) => {
		trace(parent);
		[];
	}
);

overload static function compile(ctx: GenCtx, sender: Type, msg: ObjMessage, wantValue: Bool, isSuper: Bool = false) return msg._match(
	at(Lazy(_)) => throw "bad",
	at(Single(kind)) => compile(ctx, sender, kind, wantValue),
	at(Multi(candidates, labels, args)) => compile(ctx, sender, candidates, args, isSuper, wantValue),
	at(Cast(target, candidates)) => compile(sender, target, candidates),
	at(Super(parent, msg)) => compile(ctx, parent, msg, wantValue, true)
);

overload static function compile(ctx: GenCtx, resType: Type, type: Type, kind: SingleStaticKind, wantValue: Bool): Opcodes return kind._match(
	at(SSInit(init)) => {
		init.native?._match(
			at(None) => throw "bad",
			at(Some({name: name})) => {
				return [ONative(name)];
			}
		);

		final res: Opcodes = [OSend_IS(world.getTypeRef(resType), init)];
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

		res.push(OSend_IM(world.getTypeRef(resType), init));
		if(!wantValue) {
			res.push(OPop);
		}
		res;
	},
	at(SSMethod(mth)) => {
		mth.native?._match(
			at(None) => throw "bad",
			at(Some({name: name})) => {
				return [ONative(name)];
			}
		);

		final res: Opcodes = [OSend_SS(world.getTypeRef(type), mth)];
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

		res.push(OSend_MS(world.getTypeRef(type), mth));
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
		[OInitTKind(world.getTypeRef(resType), id)];
	},
	at(SSTaggedCaseAlias(c)) => {
		trace("TODO");
		[];
	},
	at(SSValueCase(c)) => {
		final id = world.getID(c);
		[OInitVKind(world.getTypeRef(resType), id)];
	},
	at(SSFromTypevar(_, _, _, _)) => {
		trace("TODO");
		[];
	},
	at(SSFromParent(parent, kind2)) => {
		// TODO
		type=type.getMostSpecific().simplify(); // FIX
		final res = compile(ctx, type, /*parent.getFrom(type)*/type, kind2, wantValue);
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

overload static function compile(ctx: GenCtx, sender: Type, kind: SingleInstKind, wantValue: Bool): Opcodes return kind._match(
	at(SIMethod(mth)) => {
		mth.native?._match(
			at(None) => throw "bad",
			at(Some({name: name})) => {
				return [ONative(name)];
			}
		);

		final typeref = world.getTypeRef(sender);
		final res: Opcodes = [mth.typedBody != null && !sender.isProtocol() ? OSend_SI(typeref, mth) : OSendDynamic_SI(typeref, mth)];
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

		final typeref = world.getTypeRef(sender);
		res.push(mth.typedBody != null && !sender.isProtocol() ? OSend_MI(typeref, mth) : OSendDynamic_MI(typeref, mth));
		if(!wantValue && mth.ret._andOr(ret => ret != Pass2.STD_Void.thisType, false)) {
			res.push(OPop);
		}
		res;
	},
	at(SIMember(mem)) => {
		final id = world.getInstID(mem);
		[OGetMember(id)];
	},
	at(SIFromTypevar(tvar, _, _, kind2)) => {
		// TODO?
		compile(ctx, sender, kind2, wantValue);
	},
	at(SIFromParent(parent, kind2)) => {
		// TODO
		sender=sender.getMostSpecific().simplify(); // FIX
		final res = compile(ctx, /*parent.getFrom(sender)*/sender, kind2, wantValue);
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

overload static function compile(ctx: GenCtx, resType: Type, type: Type, candidates: Array<TypeMessage.TypeMultiCandidate>, args: Array<TExpr>, wantValue: Bool): Opcodes {
	return candidates._match(
		at([]) => throw "bad",
		at([{kind: kind, tctx: tctx}]) => {
			// TODO: generic type instanciation from type inference
			/*tctx._and(tctx => {
				type = type.getInTCtx(tctx);
			});*/

			final senderType = kind._match(
				at(MSInit(_, _)
				 | MSMemberwiseInit(_)
				 | MSTaggedCase(_, _, _, _)
				 | MSTaggedCaseAlias(_)) => resType,
				_ => type
			);
			final typeref = world.getTypeRef(senderType);
			kind._match(
				at(MSInit(init = {isMacro: true}, _)) => {
					throw "NYI";
				},
				at(MSInit(init, null)) => {
					init.native?._match(
						at(None) => throw "bad",
						at(Some({name: name})) => {
							var res = [];
							for(arg in args) {
								res = res.concat(compile(ctx, arg));
							}
							if(name == "ptr_new") {
								final elemType = senderType.getNative()._match(
									at(null) => throw "bad",
									at(NPtr(elem)) => elem.getFrom(senderType),
									_ => throw "bad"
								);
								final elemRef = world.getTypeRef(elemType);
								res.push(ONewPtr(elemRef));
							} else {
								res.push(ONative(name));
							}
							return res;
						}
					);

					final ictx = tctx._andOr(tctx => (
						if(tctx.size() > 0) {
							final _ictx = new TVarInstCtx();
							for(tv => t in tctx) {
								_ictx[world.getTVar(tv)] = genTVarEntry(ctx, tv, t);
							}
							_ictx;
						} else
							null
					), null);

					var res = [];
					for(arg in args) {
						res = res.concat(compile(ctx, arg));
					}
					res.push(OSend_IM(typeref, init, ictx));
					if(!wantValue) {
						res.push(OPop);
					}
					res;
				},
				at(MSInit(init, partial!!)) => {
					trace("TODO");
					[];
				},
				
				at(MSMethod(mth = {isMacro: true}, _)) => {
					throw "NYI";
				},
				at(MSMethod(mth, null)) => {
					mth.native?._match(
						at(None) => throw "bad",
						at(Some({name: name})) => {
							var res = [];
							for(arg in args) {
								res = res.concat(compile(ctx, arg));
							}
							res.push(ONative(name));
							return res;
						}
					);

					final ictx = tctx._andOr(tctx => (
						if(tctx.size() > 0) {
							final _ictx = new TVarInstCtx();
							for(tv => t in tctx) {
								_ictx[world.getTVar(tv)] = genTVarEntry(ctx, tv, t);
							}
							_ictx;
						} else
							null
					), null);

					var res = [];
					
					for(arg in args) {
						res = res.concat(compile(ctx, arg));
					}
					
					res.push(OSend_MS(typeref, mth, ictx));
					
					if(!wantValue && mth.ret._andOr(ret => ret != Pass2.STD_Void.thisType, false)) {
						res.push(OPop);
					}

					res;
				},
				at(MSMethod(mth, partial!!)) => {
					mth.native?._match(
						at(None) => throw "bad",
						at(Some({name: name})) => {
							throw "I'm too lazy for this";
							/*var res = [];
							for(arg in args) {
								res = res.concat(compile(ctx, arg));
							}
							res.push(ONative(name));
							return res;*/
						}
					);

					final ictx = tctx._andOr(tctx => (
						if(tctx.size() > 0) {
							final _ictx = new TVarInstCtx();
							for(tv => t in tctx) {
								_ictx[world.getTVar(tv)] = genTVarEntry(ctx, tv, t);
							}
							_ictx;
						} else
							null
					), null);

					var res = [];

					for(i in 0...mth.params.length) {
						partial.indexOf(i)._match(
							at(-1) => {
								// TODO: figure out how to make this easily work with generics
								var tvalue: TExpr = mth.params[i].tvalue ?? throw "bad";
								tvalue.t = tvalue.t.nonNull().getInTCtx(tctx).getFrom(type);
								tvalue.e._match(
									at(ELiteralCtor(type2, literal)) => {
										if(type2.hasTypevars()) {
											tvalue.e = ELiteralCtor(
												type2.getInTCtx(tctx).getFrom(type),
												literal
											);
										}
									},
									_ => {}
								);

								res = res.concat(compile(ctx, tvalue));
							},
							at(iarg) => {
								final arg = args[iarg];
								res = res.concat(compile(ctx, arg));
							}
						);
					}

					res.push(OSend_MS(typeref, mth, ictx));
					
					if(!wantValue && mth.ret._andOr(ret => ret != Pass2.STD_Void.thisType, false)) {
						res.push(OPop);
					}
					
					res;
				},

				at(MSMember(mem)) => {
					final id = world.getInstID(mem);
					var res: Opcodes = if(args.length == 1) compile(ctx, args[0]) else [];
					res.push(wantValue ? OTeeStaticMember(typeref, id) : OSetStaticMember(typeref, id));
					res;
				},

				at(MSMemberwiseInit(ms)) => {
					var res = [OInitClass(typeref)];
					// TODO: this does not actually work correctly
					for(i => mem in ms) {
						final id = world.getInstID(mem);

						res.push(ODup);
						res = res.concat(compile(ctx, args[i]));
						res.push(OSetMember(id));
					}

					if(senderType.hasDefaultInit()) {
						res.push(ODup);
						res.push(ODefaultInit(typeref));
					}

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
					trace("TODO");
					[];
				},

				at(MSTaggedCaseAlias(c)) => {
					trace("TODO");
					[];
				},

				at(MSFromTypevar(_, _, _, _)) => {
					trace("TODO");
					[];
				},
				at(MSFromParent(parent, kind)) => {
					trace("TODO");
					[];
				}
			);
		},
		_ => {
			trace("TODO");
			[];
		} 
	);
}

overload static function compile(ctx: GenCtx, sender: Type, candidates: Array<ObjMessage.ObjMultiCandidate>, args: Array<TExpr>, isSuper: Bool, wantValue: Bool): Opcodes {
	var typeref = world.getTypeRef(sender);
	return candidates._match(
		at([]) => throw "bad",
		at([{kind: kind, tctx: tctx}]) => kind._match(
			at(MIMethod(mth = {isMacro: true}, _)) => {
				if(mth.decl.isNative(NBool)) {
					mth.fuzzyName._match(
						at("yes: no:") => { // inline if-else macro method Bool#yes:no:
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
				mth.native?._match(
					at(None) => throw "bad",
					at(Some({name: name})) => {
						var res = [];
						for(arg in args) {
							res = res.concat(compile(ctx, arg));
						}
						res.push(ONative(name));
						return res;
					}
				);

				final ictx = tctx._andOr(tctx => {
					if(tctx.size() > 0) {
						final _ictx = new TVarInstCtx();
						for(tv => t in tctx) {
							_ictx[world.getTVar(tv)] = genTVarEntry(ctx, tv, t);
						}
						
						// Check for dup dvars in tvar table (which happens for some reason?)
						typeref._match(
							at(TInst(_, inst)) => {
								for(tv => _ in _ictx) tv._match(
									at(VDecl(tid), when(inst.exists(tid))) => {
										_ictx.remove(tv);
									},
									_ => {}
								);
								if(_ictx.size() > 0) _ictx else null;
							},
							_ => _ictx
						);
					} else {
						null;
					}
				}, null);

				var res = [];

				for(arg in args) {
					res = res.concat(compile(ctx, arg));
				}

				if(!sender.t.match(TTypeVar(_))) {
					final senderDecl = sender.getTypeDecl();
					if(mth.decl != senderDecl) {
						// TODO: make this recursive
						for(parent in senderDecl.getParents()) {
							if(parent.getTypeDecl() == mth.decl) {
								typeref = world.getTypeRef(parent.getFrom(sender));
							}
						}
					}
				}
				
				res.push(
					(mth.typedBody != null && !sender.isProtocol()) || mth.hidden!=null // TEMP: change to sealed later
					|| isSuper
					? OSend_MI(typeref, mth, ictx)
					: OSendDynamic_MI(typeref, mth, ictx)
				);
				
				if(!wantValue && mth.ret._andOr(ret => ret != Pass2.STD_Void.thisType, false)) {
					res.push(OPop);
				}
				
				res;
			},
			at(MIMethod(mth, partial!!)) => {
				mth.native?._match(
					at(None) => throw "bad",
					at(Some({name: name})) => {
						throw "I'm too lazy for this";
						/*var res = [];
						for(arg in args) {
							res = res.concat(compile(ctx, arg));
						}
						res.push(ONative(name));
						return res;*/
					}
				);

				final ictx = tctx._andOr(tctx => (
					if(tctx.size() > 0) {
						final _ictx = new TVarInstCtx();
						for(tv => t in tctx) {
							_ictx[world.getTVar(tv)] = genTVarEntry(ctx, tv, t);
						}
						// Check for dup dvars in tvar table (which happens for some reason?)
						typeref._match(
							at(TInst(_, inst)) => {
								for(tv => _ in _ictx) tv._match(
									at(VDecl(tid), when(inst.exists(tid))) => {
										_ictx.remove(tv);
									},
									_ => {}
								);
								if(_ictx.size() > 0) _ictx else null;
							},
							_ => _ictx
						);
					} else
						null
				), null);

				var res = [];

				for(i in 0...mth.params.length) {
					partial.indexOf(i)._match(
						at(-1) => {
							// Imma just hope we don't need generics here yet
							var tvalue: TExpr = mth.params[i].tvalue ?? throw "bad";
							tvalue.t = tvalue.t.nonNull().getInTCtx(tctx);//.getFrom(type);
							tvalue.e._match(
								at(ELiteralCtor(type2, literal)) => {
									if(type2.hasTypevars()) {
										tvalue.e = ELiteralCtor(
											type2.getInTCtx(tctx),//.getFrom(type),
											literal
										);
									}
								},
								_ => {}
							);

							res = res.concat(compile(ctx, tvalue));
						},
						at(iarg) => {
							final arg = args[iarg];
							res = res.concat(compile(ctx, arg));
						}
					);
				}

				if(!sender.t.match(TTypeVar(_))) {
					final senderDecl = sender.getTypeDecl();
					if(mth.decl != senderDecl) {
						// TODO: make this recursive
						for(parent in senderDecl.getParents()) {
							if(parent.getTypeDecl() == mth.decl) {
								typeref = world.getTypeRef(parent.getFrom(sender));
							}
						}
					}
				}

				res.push(
					(mth.typedBody != null && !sender.isProtocol()) || mth.hidden!=null // TEMP: change to sealed later
					|| isSuper
					? OSend_MI(typeref, mth, ictx) : OSendDynamic_MI(typeref, mth, ictx)
				);
				
				if(!wantValue && mth.ret._andOr(ret => ret != Pass2.STD_Void.thisType, false)) {
					res.push(OPop);
				}
				
				res;
			},
			at(MIMember(mem)) => {
				final id = world.getInstID(mem);
				var res: Opcodes = if(args.length == 1) compile(ctx, args[0]) else [];
				res.push(wantValue ? OTeeMember(id) : OSetMember(id));
				res;
			},
			at(MIFromTypevar(_, _, _, _)) => {
				trace("TODO");
				[];
			},
			at(MIFromParent(parent, kind)) => {
				trace("TODO");
				[];
			}
		),
		_ => {
			trace("TODO");
			[];
		} 
	);
}

overload static function compile(sender: Type, target: Type, candidates: Array<CastKind>): Opcodes {
	final typeref = world.getTypeRef(sender);
	return candidates._match(
		at([CMethod(mth, tctx)]) => {
			mth.native?._match(
				at(None) => throw "bad",
				at(Some({name: name})) => name._match(
					at("value_unsafe_cast") => {
						return [ONativeCast(world.getTypeRef(target))];
					},
					at("cast_u64_ptr") => {
						final elemType = target.getNative()._match(
							at(null) => throw "bad",
							at(NPtr(elem)) => elem.getFrom(target),
							_ => throw "bad"
						);
						final elemRef = world.getTypeRef(elemType);
						return [OPtrFromAddr(elemRef)];
					},
					_ => {
						return [ONative(name)];
					}
				)
			);

			final ictx = tctx._andOr(tctx => (
				if(tctx.size() > 0) {
					final _ictx = new TVarInstCtx();
					for(tv => t in tctx) {
						_ictx[world.getTVar(tv)] = genTVarEntry(/*BAD*/untyped null, tv, t);
					}
					_ictx;
				} else
					null
			), null);

			// TODO
			if(mth.decl is TypeVar) {
				if(target.t.match(TTypeVar(_))) {
					return [ODynamicCast(world.getTypeRef(target))];
				}
			}

			[mth.typedBody != null && !sender.isProtocol() ? OSend_C(typeref, mth, ictx) : OSendDynamic_C(typeref, mth, ictx)];
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

overload static function compile(ctx: GenCtx, sender: Type, kinds: Array<TExpr.BinaryOpCandidate>, right: TExpr, wantValue: Bool): Opcodes {
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
								if(name != "value_eq")
									trace("???", name, tctx.display(), right.orig.nonNull().mainSpan().display());
							});
						}
					);
				}, {
					final ictx = tctx._andOr(tctx => (
						if(tctx.size() > 0) {
							final _ictx = new TVarInstCtx();
							for(tv => t in tctx) {
								_ictx[world.getTVar(tv)] = genTVarEntry(ctx, tv, t);
							}
							_ictx;
						} else
							null
					), null);

					final typeref = world.getTypeRef(sender);
					res.push(mth.typedBody != null ? OSend_BO(typeref, mth, ictx) : OSendDynamic_BO(typeref, mth, ictx));
				});
			}

			if(!wantValue) {
				res.push(OPop);
			}

			res;
		},
		_ => throw "NYI "+right.orig.mainSpan().display()
	);
}

static function compilePrefix(sender: Type, kind: UnaryOpKind, wantValue: Bool) {
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
		final typeref = world.getTypeRef(sender);
		res.push(mth.typedBody != null ? OSend_UO(typeref, mth) : OSendDynamic_UO(typeref, mth));
	});

	if(!wantValue) {
		res.push(OPop);
	}

	return res;
}

// TODO: fix incr/decr ops
static function compileSuffix(sender: Type, kind: UnaryOpKind, wantValue: Bool) {
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
		final typeref = world.getTypeRef(sender);
		res.push(mth.typedBody != null ? OSend_UO(typeref, mth) : OSendDynamic_UO(typeref, mth));
	});

	if(!wantValue) {
		res.push(OPop);
	}/* else if(isStep) {
		res.push(OSwap);
		res.push(OPop);
	}*/

	return res;
}


static function genTVarEntry(ctx: GenCtx, tv: TypeVar, t: Type): Tuple2<TypeRef, Null<TVarMappings>> {
	final tref = world.getTypeRef(t);
	
	var mappings: Null<TVarMappings> = null;
	if(tv.inits.length != 0
	|| tv.members.length != 0
	|| tv.methods.length != 0
	|| tv.operators.length != 0
	|| tv.staticMembers.length != 0
	|| tv.staticMethods.length != 0
	|| tv.taggedCases.length != 0
	|| tv.valueCases.length != 0
	|| tv.categories.length != 0
	) {
		final m = new TVarMappings();

		if(tv.inits.length != 0) {

		}

		if(tv.members.length != 0) {

		}

		if(tv.methods.length != 0) {
			for(mth in tv.methods) mth._match(
				at(sm is SingleMethod) => {
					
				},
				at(mm is MultiMethod) => {

				},
				at(cm is CastMethod) => {
					detuple(@final [de, me] = world.get(cm));
					// TODO
					var res = t.findCast(null/*BAD*/, cm.type, t);
					res = typing.CastKind.reduceOverloads(res, t, cm.type);
					res._match(
						at([CMethod(cm2, _)]) => {
							detuple(@final [de2, me2] = world.get(cm2));
							m.castMethods[me.id] = tuple(de2.id, me2.id);
						},
						_ => throw "bad"
					);

					//m.castMethods.set(me.id, )
				},
				_ => throw "bad"
			);
		}

		if(tv.operators.length != 0) {

		}

		if(tv.staticMembers.length != 0) {

		}

		if(tv.staticMethods.length != 0) {

		}

		if(tv.taggedCases.length != 0) {

		}

		if(tv.valueCases.length != 0) {

		}


		if(tv.categories.length != 0) throw "NYI!";

		mappings = m;
	}

	return tuple(tref, mappings);
}


}