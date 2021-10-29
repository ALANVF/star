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

	override function findType(path: LookupPath, search: Search, from: Null<ITypeDecl>, depth = 0, cache: List<{}> = Nil): Option<Type> {
		if(cache.contains(this)) return None;
		
		switch primary {
			case None:
			case Some(p): if(!cache.contains(p)) switch p.findType(path, Inside, from, 0, cache.prepend(this)) {
				case None: cache = cache.prepend(p);
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
		if(res.isSome()) return res;
		return if(search == Inside) {
			None;
		} else {
			var a = outer.findType(path, Outside, from, depth, cache.prepend(this));
			//trace(path.simpleName(), path.span().display());
			a;
		}
	}


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