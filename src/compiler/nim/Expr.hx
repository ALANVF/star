package compiler.nim;

import compiler.nim.AnyType;
import compiler.nim.Wrap.*;

typedef Except = {cases: Exprs, body: Stmts};

@:using(compiler.nim.Expr.ExprTools)
enum Expr {
	ESym(sym: String);
	EName(name: String);
	
	EInt(i: Int, ?suffix: String);
	EFloat(i: Int, d: String, ?suffix: String);
	EChar(c: Char);
	EString(s: String);
	ETripleString(s: String);
	ERawString(s: String);
	EPrefixString(p: Name, s: String);
	ETrue;
	EFalse;
	ENil;
	EThis;
	EResult;
	ENaN;
	EInf;
	
	ETuple(exprs: Exprs);
	ENamedTuple(pairs: Array<{n: Name, v: Expr}>);
	EArray(exprs: Exprs);
	EComplexArray(entries: Array<{?k: Expr, v: Expr}>);
	ESeq(exprs: Exprs);
	ESet(exprs: Exprs);
	EDict(pairs: Array<{k: Expr, v: Expr}>);
	
	EParen(expr: Expr);
	EParenList(stmts: Stmts);
	EBlock(?name: Name, body: Stmts);
	
	EPrefix(op: Prefix, right: Expr);
	EInfix(left: Expr, op: Infix, right: Expr);
	EDeref(l: Expr);
	ERange(l: Expr, r: Expr);
	ERangeExcl(l: Expr, r: Expr);
	ERangeEnd(l: Expr, r: Expr);
	EIn(l: Expr, r: Expr);
	ENotIn(l: Expr, r: Expr);
	EIs(expr: Expr, type: Type);
	EIsNot(expr: Expr, type: Type);
	EOf(expr: Expr, type: Type);
	EAs(type: Type, expr: Expr);
	EAssign(l: Expr, r: Expr);
	
	EIndex(expr: Expr, index: Expr);
	EIndices(expr: Expr, indices: Exprs);
	EBraces(expr: Expr, exprs: Exprs);
	EField(expr: Expr, field: Name);
	EDotCast(expr: Expr, type: Type);
	//EStatic(expr: Expr);
	
	ECast(type: Type, expr: Expr);
	EAddr(expr: Expr);
	EUnsafeAddr(expr: Expr);
	ETypeof(expr: Expr);
	ECompiles(expr: Expr);
	EDefined(expr: Expr);
	EDefinedInScope(expr: Expr);
	EProcCall(expr: Expr);
	EDefault(type: Type);
	ENew(expr: Expr);
	ESizeOf(expr: Expr);
	ELen(expr: Expr);
	EOrd(expr: Expr);
	ELow(type: Type);
	EHigh(type: Type);
	EIsNil(expr: Expr);
	
	EIncl(l: Expr, r: Expr);
	EExcl(l: Expr, r: Expr);
	
	ECall(expr: Expr, types: TypeArgs, args: Exprs, ?noParens: Bool);
	EMethod(expr: Expr, method: Name, types: TypeArgs, args: Exprs, ?noParens: Bool);
	EInit(type: Type, args: Array<{n: Name, v: Expr}>);
	
	EIf(isWhen: Bool, cond: Expr, then: Stmts, ?elifs: Array<Elif<Stmt>>, ?else_: Stmts);
	ECase(value: Expr, cases: Array<Case<Stmt>>, ?else_: Stmts);
	ETry(body: Stmts, excepts: Array<Except>, ?finally: Stmts);
	EFor(vars: Array<Array<Name>>, in_: Expr, body: Stmts);
	
	EProc(params: Fields, ?ret: Type, ?pragmas: Pragmas, stmts: Stmts);
	EIter(params: Fields, ?ret: Type, ?pragmas: Pragmas, stmts: Stmts);
	EDo(?params: Fields, ?ret: Type, stmts: Stmts);
	
	EElif(e: Elif<Stmt>);
	EElse(e: Stmts);
	EExcept(e: Except);
	EFinally(f: Stmts);
	EOfBranch(o: Case<Stmt>);
	EStmtList(stmts: Stmts);
	
	EType(type: Type);
	EUntyped(nodes: Exprs);
}

@:publicFields
class ExprTools {
	static function toNim(self: Expr, tabs = "") return self._match(
		at(ESym(sym)) => '`$sym`',
		at(EName(name)) => name,
		
		at(EInt(i, null)) => '$i',
		at(EInt(i, suffix!!)) => '$i\'$suffix',
		
		at(EFloat(i, d, null)) => '$i.$d',
		at(EFloat(i, d, suffix!!)) => '$i.$d\'$suffix',
		
		at(EChar(c)) => '$c.char',
		
		at(EString(s)) => '"${s.escape()}"',
		at(ETripleString(s)) => '"""${s.escape(true, true)}"""',
		at(ERawString(s)) => 'r"${s.replaceAll('"', '""')}"',
		at(EPrefixString(prefix, s)) => '$prefix"${s.replaceAll('"', '""')}"',
		
		at(ETrue) => "true",
		at(EFalse) => "false",
		at(ENil) => "nil",
		at(EThis) => "this",
		at(EResult) => "result",
		at(ENaN) => "NaN",
		at(EInf) => "Inf",
		
		at(ETuple([expr])) => '(${expr.toNim(tabs)},)',
		at(ETuple(exprs)) => "(" + exprs.toNim(tabs) + ")",
		
		at(ENamedTuple(pairs)) => "(" + pairs.joinMap(", ", p -> p.n.toNim() + ": " + p.v.toNim(tabs)) + ")",
		
		at(EArray(exprs)) => "[" + exprs.toNim(tabs) + "]",
		
		at(EComplexArray(entries)) => "[" + entries.joinMap(", ", e -> e._match(
			at({k: k!, v: v}) => k.toNim(tabs) + ": " + v.toNim(tabs),
			at({v: v}) => v.toNim(tabs)
		)) + "]",
		
		at(ESeq(exprs)) => "@[" + exprs.toNim(tabs) + "]",
		
		at(ESet(exprs)) => "{" + exprs.toNim(tabs) + "}",
		
		at(EDict(pairs)) => "{" + pairs.joinMap(", ", p -> p.k.toNim(tabs) + ": " + p.v.toNim(tabs)) + "}",
		
		at(EParen(expr)) => "(" + expr.toNim(tabs) + ")",
		
		at(EParenList(stmts)) => "(" + stmts.toNim(tabs) + ")",
		
		at(EBlock(null, stmts)) => "block:" + stmts.toNim(tabs),
		at(EBlock(name!!, stmts)) => 'block ${name.toNim()}:' + stmts.toNim(tabs),
		
		at(EPrefix(PNot, r)) => 'not(${r.toNim(tabs)})',
		at(EPrefix(PAt, r = ESym(_) ... EParenList(_))) => "@" + r.toNim(tabs),
		at(EPrefix(PAt, r)) => '@(${r.toNim(tabs)})',
		at(EPrefix(p, r =
			( (ESym(_) ... EParenList(_))
			| EDeref(_)
			| (EIndex(_, _) ... EExcl(_, _))
			| ECall(_, _, _, null | false)
			| EMethod(_, _, _, _, null | false)
			| EInit(_, _)
		))) => p + r.toNim(tabs),
		at(EPrefix(p, r)) => '$p(${r.toNim(tabs)})',
		
		at(EInfix(l, op, r)) => wrapInfix(op, l, true).toNim(tabs) + ' ${op.toNim()} ' + wrapInfix(op, r).toNim(tabs),
		
		at(EDeref(l)) => wrapImmediate(l).toNim(tabs) + "[]",
		
		at(ERange(l, r)) => EInfix(l, IRange, r).toNim(tabs),
		at(ERangeExcl(l, r)) => EInfix(l, IRangeExcl, r).toNim(tabs),
		at(ERangeEnd(l, r)) => EInfix(l, IRangeEnd, r).toNim(tabs),
		at(EIn(l, r)) => EInfix(l, IOp("in"), r).toNim(tabs),
		at(ENotIn(l, r)) => EInfix(l, IOp("not_in"), r).toNim(tabs),
		at(EIs(e, t)) => wrapInfix(e, true).toNim(tabs) + " is " + wrapInfixType(t).toNim(tabs),
		at(EIsNot(e, t)) => wrapInfix(e, true).toNim(tabs) + " is_not " + wrapInfixType(t).toNim(tabs),
		at(EOf(e, t)) => wrapInfix(e, true).toNim(tabs) + " of " + wrapInfixType(t).toNim(tabs),
		at(EAs(t, e)) => wrapInfixType(t, true).toNim(tabs) + " as " + wrapInfix(e).toNim(tabs),
		at(EAssign(l, r)) => EInfix(l, IOp("="), r).toNim(tabs),
		
		at(EIndex(e, i)) => wrapImmediate(e).toNim(tabs) + "[" + i.toNim(tabs) + "]",
		at(EIndices(e, ia)) => wrapImmediate(e).toNim(tabs) + "[" + ia.toNim(tabs) + "]",
		at(EBraces(e, ea)) => wrapImmediate(e).toNim(tabs) + "{" + ea.toNim(tabs) + "}",
		at(EField(e, f)) => wrapImmediate(e).toNim(tabs) + "." + f.toNim(),
		at(EDotCast(e, t)) => wrapImmediate(e).toNim(tabs) + "." + wrapImmediateType(t).toNim(tabs),
		
		at(ECast(t, e)) => 'cast[${t.toNim(tabs)}](${e.toNim(tabs)})',
		at(EAddr(e)) => 'addr(${e.toNim(tabs)})',
		at(EUnsafeAddr(e)) => 'unsafeAddr(${e.toNim(tabs)})',
		at(ETypeof(e)) => 'typeof(${e.toNim(tabs)})',
		at(ECompiles(e)) => 'compiles(${e.toNim(tabs)})',
		at(EDefined(e)) => 'defined(${e.toNim(tabs)})',
		at(EDefinedInScope(e)) => 'definedInScope(${e.toNim(tabs)})',
		at(EProcCall(e)) => 'procCall(${e.toNim(tabs)})',
		at(EDefault(e)) => 'default(${e.toNim(tabs)})',
		at(ENew(e)) => 'new(${e.toNim(tabs)})',
		at(ESizeOf(e)) => 'sizeof(${e.toNim(tabs)})',
		at(ELen(e)) => 'len(${e.toNim(tabs)})',
		at(EOrd(e)) => 'ord(${e.toNim(tabs)})',
		at(ELow(e)) => 'low(${e.toNim(tabs)})',
		at(EHigh(e)) => 'high(${e.toNim(tabs)})',
		at(EIsNil(e)) => 'isNil(${e.toNim(tabs)})',
		
		at(EIncl(l, r)) => 'incl(${l.toNim(tabs)}, ${r.toNim(tabs)})',
		at(EExcl(l, r)) => 'excl(${l.toNim(tabs)}, ${r.toNim(tabs)})',
		
		at(ECall(expr, types, args, noParens)) => {
			var res = wrapImmediate(expr).toNim(tabs);
			
			if(types.length != 0) {
				res += types.toNim(tabs);
			}
			
			if(args.length == 0) {
				res += "()";
			}
			
			final firstNonArg = args.findIndex(a -> a._match(
				at(EDo(_, _, _) ... EStmtList(_)) => true,
				_ => false
			));
			if(firstNonArg == -1) {
				final _args = args.joinMap(", ", a -> a.toNim(tabs));
				res += noParens ? ' $_args' : '($_args)';
			} else {
				if(firstNonArg == 0) {
					if(!noParens) res += "()";
					else switch args[firstNonArg] {
						case EStmtList(_):
						case EOfBranch(_): res += '\n$tabs';
						default: res += " ";
					}
				} else {
					final _args = args.slice(0, firstNonArg).joinMap(", ", a -> a.toNim(tabs));
					res += noParens ? ' $_args' : '($_args)';
					switch args[firstNonArg] {
						case EStmtList(_):
						case EOfBranch(_): res += '\n$tabs';
						default: res += " ";
					}
				}
				
				res += args[firstNonArg].toNim(tabs);
				
				for(i in (firstNonArg + 1)...args.length) {
					res += '\n$tabs';
					res += args[i].toNim(tabs);
				}
			}
			
			res;
		},
		at(EMethod(expr, method, types, args, parens)) => ECall(EField(expr, method), types, args, parens).toNim(tabs),
		at(EInit(type, args)) => wrapImmediateType(type) + "(" + args.joinMap(", ", a -> a.n.toNim() + ": " + a.v.toNim(tabs)) + ")",
		
		at(EIf(when, cond, then, elifs, else_)) => {
			final args = [cond, EStmtList(then)];
			elifs._and(el => args.pushAll(el.map(e -> EElif(e))));
			else_._and(e => args.push(EElse(e)));
			ECall(EName(when ? "when" : "if"), [], args, true).toNim(tabs);
		},
		at(ECase(value, cases, else_)) => {
			final args = [value].concat(cases.map(c -> EOfBranch(c)));
			else_._and(e => args.push(EElse(e)));
			ECall(EName("case"), [], args, true).toNim(tabs);
		},
		at(ETry(body, excepts, finally)) => {
			final args = [EStmtList(body)].concat(excepts.map(e -> EExcept(e)));
			finally._and(f => args.push(EFinally(f)));
			ECall(EName("try"), [], args, true).toNim(tabs);
		},
		at(EFor(vars, in_, body)) => {
			final args = vars.map(v -> switch v {
				case [n]: n.toExpr();
				default: ETuple(v.map(n -> n.toExpr()));
			});
			args.setLast(EIn(args.last(), in_));
			args.push(EStmtList(body));
			ECall(EName("for"), [], args, true).toNim(tabs);
		},
		
		at(EProc(params, ret, pragmas, stmts)) => {
			var res = 'proc(${params.paramsToNim(tabs)})';
			ret._and(r => res += ": " + r.toNim(tabs));
			pragmas._and(p => res += " " + p.toNim());
			res += " =" + stmts.toNim(tabs);
			res;
		},
		at(EIter(params, ret, pragmas, stmts)) => {
			var res = 'iterator(${params.paramsToNim(tabs)})';
			ret._and(r => res += ": " + r.toNim(tabs));
			pragmas._and(p => res += " " + p.toNim());
			res += " =" + stmts.toNim(tabs);
			res;
		},
		at(EDo(params, ret, stmts)) => {
			var res = "do";
			params._and(p => res += " " + p.paramsToNim(tabs));
			ret._and(r => res += " -> " + r.toNim(tabs));
			res += ":" + stmts.toNim(tabs);
			res;
		},
		
		at(EElif(e)) => ECall(EName("elif"), [], [e.cond, EStmtList(e.body)], true).toNim(tabs),
		at(EElse(e)) => ECall(EName("else"), [], [EStmtList(e)], true).toNim(tabs),
		at(EExcept(e)) => ECall(EName("except"), [], e.cases.concat([EStmtList(e.body)]), true).toNim(tabs),
		at(EFinally(f)) => ECall(EName("finally"), [], [EStmtList(f)], true).toNim(tabs),
		at(EOfBranch(o)) => o._match(
			at(COf(v, b)) => ECall(EName("of"), [], [v, EStmtList(b)], true),
			at(COfAny(vs, b)) => ECall(EName("of"), [], vs.concat([EStmtList(b)]), true),
			at(CElif(c, b)) => ECall(EName("elif"), [], [c, EStmtList(b)], true)
		).toNim(tabs),
		at(EStmtList(stmts)) => ":" + stmts.toNim(tabs),
		
		at(EType(t)) => t.toNim(tabs),
		at(EUntyped(nodes)) => nodes.toNim(tabs, " ")
	);
}