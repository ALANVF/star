package util;

import haxe.macro.Expr;
import haxe.macro.Context;

class Auto {
	public macro static function build(?options: {init: Bool}): Array<Field> {
		final optInit = options == null || options.init;
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
		if(thisFields.find(f -> f.name == "new") == null) {
			var fields: Array<Field> = [];
			var inits = [];
			var isOptional = true;
			
			for(f in thisFields) {
				final getThis = ["this", f.name];
				final getInit = ["init", f.name];

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
			}

			if(thisClass.superClass != null) {
				var currentSuper = thisClass.superClass;
				var superIsOptional = false;
				
				while(currentSuper != null) {
					switch currentSuper {
						case {t: _.get() => superClass}: //if(superClass.meta.has(":auto")):
							currentSuper = superClass.superClass;
							switch superClass.constructor {
								case null: break;
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
						
						default: break;
					}
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
		
		return thisFields;
	}
}