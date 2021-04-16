import util.Buffer;
import util.List;
import haxe.macro.Context;
import haxe.macro.Expr;

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

	@:noUsing
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
			while(_case.expr.match(EDisplay(_, _))) switch _case {
				case {expr: EDisplay(e, _)}: _case = e;
				default: break;
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

		var type = Context.typeof(value);

		while(type.match(TType(_, _))) switch type {
			case TType(_.get() => t, _): type = t.type;
			default: break;
		}

		switch type {
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

	private static function mapListPattern(pattern: Expr, isOuter = false) return switch pattern {
		case {expr: EDisplay(e, _)}: macro ${mapListPattern(e, isOuter)};
		case macro $l | $r: macro ${mapListPattern(l)} | ${mapListPattern(r)};
		case macro []: macro Nil;
		case macro [$a{values}]: macro ${listOf(values)};
		default: pattern;
	}

	private static function mapPattern(pattern: Expr, isOuter = false) return switch pattern {
		case {expr: EDisplay(e, _)}: macro ${mapPattern(e, isOuter)};
		case macro [$a{values}]: macro $a{values.map(v -> mapPattern(v))};
		case macro $l | $r: macro ${mapPattern(l)} | ${mapPattern(r)};
		default: pattern;
	}

	private static function listOf<T>(values: Array<Expr>): ExprOf<List<T>> {
		if(values.length == 0) {
			return macro Nil;
		} else {
			return switch switch values.last() {
				case {expr: EDisplay(e, _)}: e;
				case e: e;
			} {
				case macro ...$rest = $expr:
					values.pop();
					macro ${values.foldRight(macro $rest = ${mapListPattern(expr)}, (acc, v) -> macro Cons($v, $acc))};

				case macro ...$rest:
					values.pop();
					macro ${values.foldRight(rest/*mapListPattern(rest)*/, (acc, v) -> macro Cons($v, $acc))};
				
				default:
					macro ${values.foldRight(macro Nil, (acc, v) -> macro Cons($v, $acc))};
			}
		}
	}

	private static function _pretty(value: Any, indent: Int, tab: String, nested: List<Any>): String {
		final thisLevel = tab.repeat(indent);
		final nextLevel = tab.repeat(indent + 1);
		
		return if(value is Array) {
			final array = (value : Array<Any>);

			if(array.length == 0) {
				"[]";
			} else {
				var out = new Buffer();
				
				out.add("[\n");
				
				for(i in 0...array.length) {
					out.add(nextLevel);
					out.add(_pretty(array[i], indent + 1, tab, nested));
					if(i < array.length - 1) {
						out.add(",");
					}
					out.add("\n");
				}

				out.add('$thisLevel]');

				out.toString();
			}
		} else if(value is List) {
			_pretty((value : List<Any>).toArray(), indent, tab, nested);
		} else if(value is String) {
			final str = (value : String);
			
			str.quoteDouble().replaceAll("\n", "\\n").replaceAll("\r", "\\r").replaceAll("\t", "\\t");
		} else if(Reflect.isEnumValue(value)) {
			final value = (value : EnumValue);
			final name = value.getName();

			switch value.getParameters() {
				case []: name;
				case [param]: '$name(${_pretty(param, indent, tab, nested)})';
				case params: '$name(\n' + params.map(param -> nextLevel + _pretty(param, indent + 1, tab, nested)).join(",\n") + '\n$thisLevel)';
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
							out.add(_pretty(Reflect.field(value, field), indent + 1, tab, nested));
							out.add(",\n");
						}

						out.add(nextLevel);
						out.add(last);
						out.add(": ");
						out.add(_pretty(Reflect.field(value, last), indent + 1, tab, nested));
						
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
							if(val == value || nested.contains(val)) {
								out.add("...");
							} else {
								out.add(_pretty(val, indent + 1, tab, nested.prepend(val)));
							}
							out.add(",\n");
						}

						final val = Reflect.getProperty(value, last);

						if(!Reflect.isFunction(val)) {
							out.add(nextLevel);
							out.add(last);
							out.add(": ");
							if(val == value || nested.contains(val)) {
								out.add("...");
							} else {
								out.add(_pretty(val, indent + 1, tab, nested.prepend(val)));
							}
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
		return _pretty(value, 0, tab, Cons(value, Nil));
	}

	static inline function parseInt(str: String) {
		return nonNull(Std.parseInt(str));
	}
	
	static function parseOctal(str: String) {
		var int = 0;

		for(i in 0...str.length) {
			final char = (str.charCodeAt(i) : Char);

			int *= 8;
			int += char - 48;
		}

		return int;
	}
	
	static function parseHex(str: String) {
		var int = 0;

		for(i in 0...str.length) {
			final char = (str.charCodeAt(i) : Char);

			int *= 16;
			int += char - switch char {
				case '0'.code,49,50,51,52,53,54,55,56,'9'.code: 48;
				case 'A'.code,66,67,68,69,'F'.code: 55;
				case 'a'.code,98,99,100,101,'f'.code: 87;
				default: throw "error!";
			};
		}

		return int;
	}
}