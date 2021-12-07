package util;

import haxe.macro.Expr;
import haxe.macro.Context;

using haxe.macro.ExprTools;
using haxe.macro.TypedExprTools;
using haxe.macro.TypeTools;

class Overload {
	#if macro
	/*
	 * Z_ = sep
	 * Zd = path sep
	 * ZZ = "Z"
	 * Zg = type path w/ generic args
	 * Zf = function type
	 * Zm = method type
	 * Za = anon type
	 * ZA = extended anon type
	 * Zp = paren(t) type
	 * Zo = optional type
	 * ZO = optional function param
	 * Zn = named type
	 * Zi = intersection type
	 * Zu = unknown
	 * ZT = function type params
	 * ZP = function params
	 * Zc = constrained type param
	*/
	static function maybeMangleType(type: Null<ComplexType>): String {
		return if(type == null) "Zu" else mangleType(type);
	}
	static function mangleName(name: String): String {
		return name.replaceAll("Z", "ZZ");
	}
	static function mangleTypePath(path: TypePath): String {
		// TODO: determine the full pack for imported/aliased/local type paths
		var res = mangleName(path.name);

		if(path.sub != null) {
			if(path.name == "StdTypes" && path.pack.length == 0) {
				res = mangleName(path.sub);
			} else {
				res += "Zd" + mangleName(path.sub);
			}
		}

		res = path.pack.joinMap("", p -> mangleName(p) + "Zd") + res;

		if(path.params != null && path.params.length > 0) {
			res = "Zg" + res
				+ "Z_" + path.params.length
				+ path.params.joinMap("Z_", p -> switch p {
					case TPType(t): mangleType(t);
					case TPExpr(e): throw "Cannot mangle expressions! (yet)";
				});
		}

		return res;
	}
	static function mangleType(type: ComplexType): String {
		return switch type {
			case TPath(path): mangleTypePath(path);
			
			case TFunction(args, ret):
				"Zf" + args.length
				+ args.joinMap("Z_", mangleType)
				+ mangleType(ret);
			
			case TAnonymous(fields): // worry about meta later lol
				"Za" + fields.length
				+ fields.joinMap("Z_", f -> f.name + "Z_" + switch f.kind {
					case FVar(t, _): maybeMangleType(t);
					case FFun(f):
						"Zm" + f.args.length
						+ f.args.joinMap("Z_", a -> maybeMangleType(a.type))
						+ maybeMangleType(f.ret);
					case FProp(_, _, t, _): maybeMangleType(t);
				});
			
			case TParent(t): "Zp" + mangleType(t);

			case TExtend(ps, fields):
				"ZA"
				+ ps.length
				+ ps.joinMap("Z_", mangleTypePath)
				+ "Z_"
				+ fields.length
				+ fields.joinMap("Z_", f -> f.name + "Z_" + switch f.kind {
					case FVar(t, _): maybeMangleType(t);
					case FFun(f):
						"Zm" + f.args.length
						+ f.args.joinMap("Z_", a -> maybeMangleType(a.type))
						+ maybeMangleType(f.ret);
					case FProp(_, _, t, _): maybeMangleType(t);
				});
			
			case TOptional(t): "To" + mangleType(t);

			case TNamed(n, t): "Tn" + n + "Z_" + mangleType(t);

			case TIntersection(tl):
				"Zi" + tl.length
				+ tl.joinMap("Z_", mangleType);
		}
	}
	#end

	public macro static function build(): Array<Field> {
		final thisClass = switch Context.getLocalClass() {
			case null: Context.fatalError("Overload must be used on a class!", Context.currentPos());
			case _.get() => t: t;
		};
		
		final thisFields = Context.getBuildFields();
		
		return thisFields.flatMap(function(field) {
			if((field.meta != null && field.meta.some(m -> m.name == "ignore" || m.name == ":overload" || m.name == "overload_processed"))
			|| field.name == "new" || field.name == "__init__"
			|| !field.kind.match(FFun(_))
			|| !field.access.contains(AOverload)
			|| field.access.contains(AExtern) || field.access.contains(ADynamic) || field.access.contains(AMacro)
			) {
				return [field];
			}

			final f = switch field.kind {
				case FFun(f_): f_;
				default: throw "???";
			};

			final access = field.access != null ? field.access : [];
			final meta = field.meta != null ? field.meta : [];

			var mangledName = mangleName(field.name);
			if(f.params != null && f.params.length > 0) {
				// TODO: don't depend on type param names/ordering
				mangledName += (
					"ZT" + f.params.length
					+ f.params.joinMap("Z_", p -> { // worry about meta later lol
						var res = mangleName(p.name);
						if(p.params != null && p.params.length > 0) throw "WHAT MAGIC IS THIS???";
						if(p.constraints != null && p.constraints.length > 0) {
							res = (
								"Zc" + p.constraints.length
								+ res
								+ p.constraints.joinMap("", c -> "Z_" + mangleType(c))
							);
						}
						res;
					})
				);
			}
			mangledName += (
				"ZA" + f.args.length
				+ f.args.joinMap("Z_", a -> {
					var t =
						if(a.type != null)
							a.type
						else if(a.value != null)
							Context.toComplexType(Context.typeof(a.value))
						else null;
					var res = maybeMangleType(t);
					if(a.opt == true || a.value != null) res = 'ZO$res';
					res;
				})
			);

			final argNames = f.args.map(a -> macro $i{a.name});

			var result: Array<Field> = [];

			// extern inline overload
			if(!access.contains(AOverride)) {
				result.push({
					name: field.name,
					doc: field.doc,
					access: {
						final a = access.copy();
						a.remove(AOverride);
						a.remove(AFinal);
						a.push(AExtern);
						if(!a.contains(AInline)) a.push(AInline);
						a;
					},
					kind: FFun({
						params: f.params,
						args: f.args,
						ret: f.ret,
						expr: switch f.ret {
							case TPath({pack: [], name: "Void"}): {
								macro $i{mangledName}($a{argNames});
							}
							default: {
								macro return $i{mangledName}($a{argNames});
							}
						}
					}),
					pos: field.pos,
					meta: meta
				});
			}

			result.push({
				name: mangledName,
				access: {
					final a = access.copy();
					a.remove(AOverload);
					a.remove(APublic);
					a.push(APrivate);
					a;
				},
				kind: field.kind,
				pos: field.pos,
				meta: meta.concat([{
					name: "overload_processed",
					pos: field.pos
				}])
			});

			return result;
		});
	}
}