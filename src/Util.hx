import util.Buffer;
import haxe.macro.Context;
import haxe.macro.Expr;

using hx.strings.Strings;

@:publicFields
class Util {
	static macro function assert(expr) {
		return macro {
			if(!($expr)) {
				throw new AssertionError('${haxe.macro.ExprTools.toString(expr)}');
			}
		};
	}

	static macro function ifMatch(value, pattern, expr) {
		return macro {
			switch($value) {
				case $pattern: $expr;
				case _:
			}
		}
	}

	static macro function extract(value, pattern, expr) {
		return macro {
			switch($value) {
				case $pattern: $expr;
				default: throw "Match error!";
			}
		}
	}

	static inline function nonNull<T>(value: Null<T>): T {
		if(value != null)
			return value;
		else
			throw new NullException();
	}

	static macro function match<T>(value: ExprOf<T>, cases: Array<Expr>): Expr {
		/*final elseExpr = if(cases.length % 2 != 0) {
			Some(switch (cases.pop() : Expr) {
				case macro @else $expr: expr;
				case _: throw "error!";
			});
		} else {
			None;
		};*/

		var defaultExpr = None;
		var caseExprs: Array<Case> = [];
		
		for(_case in cases) {
			switch _case {
				case {expr: EDisplay(e, _)}: _case = e;
				default:
			}

			switch _case {
				case macro at($pattern, when($cond)) => $expr: caseExprs.push({
					values: [pattern],
					guard: cond,
					expr: expr
				});

				case macro at($pattern) => $expr: caseExprs.push({
					values: [pattern],
					expr: expr
				});

				case macro _ => $expr: defaultExpr = Some(expr);

				default: throw "error!";
			};
		}

		switch Context.typeof(value) {
			case TEnum(_.get() => {pack: ["util"], name: "List"}, [_]):
				caseExprs = caseExprs.map(_case -> switch _case {
					case {values: values, guard: guard, expr: expr}: {
						values: values.map(value -> mapListPattern(value, true)),
						guard: guard,
						expr: expr
					};
				});
			default:
		}

		return {
			expr: ESwitch(value, caseExprs, defaultExpr.toNull()),
			pos: Context.currentPos()
		};
	}

	private static function mapListPattern(pattern: Expr, isOuter = false) {
		final expr = switch pattern {
			case macro $l | $r: macro ${mapListPattern(l)} | ${mapListPattern(r)};
			case macro []: macro Nil;
			//case {expr: EDisplay(macro [], k), pos: pos}: {expr: EDisplay(macro Nil, k), pos: pos};
			case macro [$a{values}]: macro ${listOf(values)};
			//case {expr: EDisplay({expr: EArrayDecl(values)}, k), pos: pos}: {expr: EDisplay(macro ${listOf(values.map(v -> mapPattern(v)))}, k), pos: pos};
			default: pattern;
		};

		return expr;/*{
			expr: if(pattern.expr.match(EDisplay(_, _) | EDisplayNew(_))) {
				expr;
			} else {
				EDisplay(expr, DKPattern(isOuter));
			},
			pos: pattern.pos
		}*/
	}

	private static function mapPattern(pattern: Expr, isOuter = false) return {
		expr: EDisplay(switch pattern {
			case macro [$a{values}]: macro $a{values.map(v -> mapPattern(v))};
			case macro $l | $r: macro ${mapPattern(l)} | ${mapPattern(r)};
			default: pattern;
		}, DKPattern(isOuter)),
		pos: pattern.pos
	}

	private static function listOf<T>(values: Array<Expr>): ExprOf<List<T>> {
		if(values.length == 0) {
			return macro Nil;
		} else {
			return switch values.last() {
				case (macro @rest $rest) | macro @_ $rest:
					values.pop();
					macro ${values.foldRight(rest, (acc, v) -> macro Cons($v, $acc))};
				
				default:
					macro ${values.foldRight(macro Nil, (acc, v) -> macro Cons($v, $acc))};
			}
		}
	}

	private static function _pretty(value: Any, indent: Int, tab: String): String {
		final thisLevel = tab.repeat(indent);
		final nextLevel = tab.repeat(indent + 1);
		
		return if((value is Array)) {
			final array = (value : Array<Any>);

			if(array.length == 0) {
				"[]";
			} else {
				var out = new Buffer();
				
				out.add("[\n");
				
				for(i in 0...array.length) {
					out.add(nextLevel);
					out.add(_pretty(array[i], indent + 1, tab));
					if(i < array.length - 1) {
						out.add(",");
					}
					out.add("\n");
				}

				out.add('$thisLevel]');

				out.toString();
			}
		} else if((value is List)) {
			_pretty((value : List<Any>).toArray(), indent, tab);
		} else if((value is String)) {
			final str = (value : String);
			
			str.quoteDouble().replaceAll("\n", "\\n").replaceAll("\r", "\\r").replaceAll("\t", "\\t");
		} else if(Reflect.isEnumValue(value)) {
			final value = (value : EnumValue);
			final name = value.getName();

			switch value.getParameters() {
				case []: name;
				case [param]: '$name(${_pretty(param, indent, tab)})';
				case params: '$name(\n' + params.map(param -> nextLevel + _pretty(param, indent + 1, tab)).join(",\n") + '\n$thisLevel)';
			}
		} else {
			switch Type.typeof(value) {
				case TObject:
					final fields = Reflect.fields(value);

					if(fields.length == 0) {
						"{}";
					} else {
						final out = new Buffer();
						final last = (fields.pop() : String);

						out.add("{\n");

						for(field in fields) {
							out.add(nextLevel);
							out.add(field);
							out.add(": ");
							out.add(_pretty(Reflect.field(value, field), indent + 1, tab));
							out.add(",\n");
						}

						out.add(nextLevel);
						out.add(last);
						out.add(": ");
						out.add(_pretty(Reflect.field(value, last), indent + 1, tab));
						
						out.add("\n");
						out.add(thisLevel);
						out.add("}");

						out.toString();
					}
				
				case TClass(c):
					final fields = Type.getInstanceFields(c);
					final typeName = Type.getClassName(c);
					
					//hl.Type.getDynamic(value)
					
					if(fields.contains("toString")) {
						return Std.string(value);
					}

					if(fields.length == 0) {
						'$typeName {}';
					} else {
						final out = new Buffer();
						final last = (fields.pop() : String);

						out.add(typeName);
						out.add(" {\n");
						
						for(field in fields) {
							final val = Reflect.getProperty(value, field);

							if(val == null || Reflect.isFunction(val)) continue;

							out.add(nextLevel);
							out.add(field);
							out.add(": ");
							out.add(_pretty(val, indent + 1, tab));
							out.add(",\n");
						}

						final val = Reflect.getProperty(value, last);

						if(!Reflect.isFunction(val)) {
							out.add(nextLevel);
							out.add(last);
							out.add(": ");
							out.add(_pretty(Reflect.getProperty(value, last), indent + 1, tab));
						}
							
						out.add("\n");
						out.add(thisLevel);
						out.add("}");

						out.toString();
					}

				default:
					Std.string(value);	
			}
		}
	}

	static inline function pretty(value: Any, tab = "  "): String {
		return _pretty(value, 0, tab);
	}

	static inline function parseInt(str: String) {
		return nonNull(Std.parseInt(str));
	}
}