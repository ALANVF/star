package typing;

import typing.Traits;

class Unit extends Dir {
	final outer: Dir;
	var primary: Option<File> = None;
	//var primaryType: Option<Type> = None;

	override function gatherFiles(gather: Array<File>) {
		primary.forEach(p -> gather.push(p));
		super.gatherFiles(gather);
	}

	override function findType(path: LookupPath, search: Search, from: Null<Traits.ITypeDecl>, depth = 0, cache: List<{}> = Nil): Option<Type> {
		if(search!=Inside&&cache.contains(this)) return None;
		
		switch primary {
			case None:
			case Some(p): if(!cache.contains(p)) switch p.findType(path, Inside, from, 0, cache.prepend(this)) {
				case None:
				case Some(t) if(depth != 0):
					cache = cache.prepend(t);
					depth--;
				case Some(t): return Some(t);
			}
		}

		/*for(file in files) if(!cache.contains(file)) {
			//if(path.simpleName().contains("Tail"))trace(from._and(f => f.name.name), path.simpleName(), file.path);
			switch file.findType(path, Inside, from, 0, cache) {
				case None:
				case Some(t) if(depth != 0):
					cache = cache.prepend(t);
					depth--;
				case Some(t):
					//if(path.simpleName().contains("Head")) trace(path.span().display(), t);
					return Some(t);
			}
		}*/

		final res = super.findType(path, search, from, depth, cache);
		//if(path.simpleName() == "LinkIterator")trace(this.path, primary.exists(p -> p == cache.head()), res);
		if(res.isSome())return res;
		return res.orDo(
			if(search == Inside) {
				None;
			} else {
				var a = outer.findType(path, Outside, from, depth, cache.prepend(this));
				//trace(path.simpleName(), a);
				return a;
			}
		);
	}

	/*override function findTypeOld(path: LookupPath, absolute = false, cache: List<{}> = Nil): Option<Type> {
		if(!absolute && cache.contains(this)) return None;

		var fromHere = cache._match(
			//at(Cons(u is Unit, _)) => units.contains(u),
			at(Cons(f is File, _)) => (primary.contains(f) || files.contains(f)),
			_ => false
		);
		
		if(absolute) {
			if(cache.contains(this)) {
				return None;
			} else {
				cache = cache.prepend(this);
			}
		}
		
		return path._match(
			at([[span, typeName, _]], when(typeName == name)) => switch primary.flatMap(p -> p.findTypeOld(path, false, cache)) {
				case Some(type): Some(type.t.match(TModular(_, _)) ? type : {t: TModular(type, this), span: span});
				case None:
					super.findTypeOld(path, false, cache).orDo(
						if(absolute) outer.findTypeOld(path, true, cache) else None
					);
			},
			at([[span, typeName, _], ...rest], when(typeName == name)) => switch primary.flatMap(p -> p.findTypeOld(path, false, cache)) {
				case Some(type): Some(type.t.match(TModular(_, _)) ? type : {t: TModular(type, this), span: span});
				// lol is this dead code?
				case None: switch primary.flatMap(p -> p.findTypeOld(path, false, cache)) {
					case Some(type): Some(type);
					case None:
						super.findTypeOld(rest, false, cache.prepend(this)).orDo(
							outer.findTypeOld(path, true, cache.prepend(this))
						);
				}
			},
			_ => {
				switch primary.flatMap(p -> p.findTypeOld(path, false, cache)) {
					case Some(type) if(cache.contains(type)):
						return outer.findTypeOld(path, true, cache.prepend(this));
					case Some(type): Some(type.t.match(TModular(_, _)) ? type : {t: TModular(type, this)});
					case None:
						((absolute || !cache.some(c -> c is Project)) ? super.findTypeOld(path, false, cache.prepend(this)) : None).orDo(
							if(absolute) outer.findTypeOld(path, false, cache.prepend(this)).orDo(outer.findTypeOld(path, true, cache.prepend(this)))
							else {
								//trace(name, path.simpleName());
								//trace(path.simpleName(), name, super.findTypeOld(path, true, cache.prepend(this)));
								None;
							}
						);
				}
			}
		);
	}*/


	override function findCategory(cat: Type, forType: Type, from: ITypeDecl, cache: List<{}> = Nil): Array<Category> {
		if(cache.contains(this)) return [];
		
		final candidates = switch primary {
			case None: [];
			case Some(p): p.findCategory(cat, forType, from, cache.prepend(this));
		};

		for(file in files) {
			switch file.findCategory(cat, forType, from, cache.prepend(this)) {
				case []:
				case found: candidates.pushAll(found);
			}
		}

		for(unit in units) {
			switch unit.findCategory(cat, forType, from, cache.prepend(this)) {
				case []:
				case found: candidates.pushAll(found);
			}
		}

		return candidates;
	}
}