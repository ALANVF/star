package util;

import haxe.macro.Expr;
import haxe.macro.Context;

class Auto {
	public macro static function build(?options: {?init: Bool, ?keepInit: Bool}): Array<Field> {
		final optInit = options == null || options.init == null || options.init;
		final optKeepInit = options != null && options.keepInit != null && options.keepInit;
		final thisClass = switch Context.getLocalClass() {
			case null: Context.fatalError("Auto must be used on a class!", Context.currentPos());
			case _.get() => t: t;
		};
		
		final thisFields = Context.getBuildFields();

		for(f in thisFields) {
			if(f.access == null) {
				f.access = [APublic];
			} else if(!f.access.contains(APrivate)) {
				f.access.push(APublic);
			}
		}

		//thisClass.meta.add(":auto", [], Context.currentPos());

		// Constructor
		final oldNew = thisFields.find(f -> f.name == "new");
		if(oldNew == null || optKeepInit) {
			var fields: Array<Field> = [];
			var inits = [];
			var isOptional = true;
			final toRemove = [];

			for(f in thisFields) {
				if(f.access.contains(AStatic)) continue;

				final getThis = ["this", f.name];
				final getInit = ["init", f.name];

				if(f.meta.find(m -> m.name == "ignore") != null) {
					continue;
				}

				switch f.kind {
					case FVar(null, _):
						Context.warning('Cannot determine the type of field "${f.name}"', f.pos);

					case FVar(t, null):
						fields.push({
							name: f.name,
							kind: FVar(t, null),
							pos: Context.currentPos()
						});

						if(isOptional) isOptional = false;

						inits.push(macro $p{getThis} = $p{getInit});
					
					case FVar(t, e):
						fields.push({
							name: f.name,
							kind: FVar(t, null),
							meta: [{
								name: ":optional",
								pos: Context.currentPos()
							}],
							pos: Context.currentPos()
						});

						inits.push(macro if($p{getInit} != null) $p{getThis} = $p{getInit});
					
					default:
				}

				if(f.meta.find(m -> m.name == "fromInterface") != null) {
					toRemove.push(f);
				}
			}

			if(toRemove.length != 0) for(f in toRemove) {
				thisFields.remove(f);
			}

			if(thisClass.superClass != null) {
				var superIsOptional = false;

				switch thisClass.superClass {
					case {t: _.get() => superClass}:
						switch superClass.constructor {
							case null:
							case _.get() => ctor:
								final ctorType = switch ctor.type {
									case TLazy(f): f();
									case t: t;
								};
							
								switch ctorType {
									case TFun([{t: arg}], _):
										switch arg {
											case TAbstract(_.get().name => "Null", [t]):
												superIsOptional = true;
												arg = t;
											default:
										}

										switch arg {
											case TAnonymous(_.get() => {fields: superFields}):
												for(field in superFields) fields.push({
													name: field.name,
													meta: [for(m in field.meta.extract(":optional")) m],
													kind: FVar(Context.toComplexType(field.type), null),
													pos: Context.currentPos()
												});
											
											default:
										}

									default:
								}
						}
					
					default:
				}

				inits.unshift(macro super(cast init));
				isOptional = isOptional && superIsOptional;
			}

			if(isOptional) {
				inits = if(thisClass.superClass != null) {
					[inits.shift(), macro if(init != null) $b{inits}];
				} else {
					[macro if(init != null) $b{inits}];
				}
			}

			if(oldNew != null && optKeepInit) {
				switch oldNew.kind {
					case FFun(f):
						f.args = if(fields.length == 0) [] else [{
							name: "init",
							type: TAnonymous(fields),
							opt: isOptional
						}];
						
						f.expr = macro $b{
							inits.concat(
								switch f.expr {
									case macro $b{block}: block;
									default: [f.expr];
								}
							)
						};

					default:
				}
			} else {
				thisFields.push({
					name: "new",
					kind: FFun({
						args: if(fields.length == 0) [] else [{
							name: "init",
							type: TAnonymous(fields),
							opt: isOptional
						}],
						expr: macro $b{inits},
						params: [],
						ret: null
					}),
					access: [optInit ? APublic : APrivate],
					pos: Context.currentPos()
				});
			}
		}
		
		return thisFields;
	}
}