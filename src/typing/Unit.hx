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

	override function findType(path: LookupPath, search: Search, from: Null<AnyTypeDecl>, depth = 0, cache: Cache = Nil): Null<Type> {
		if(cache.contains(this)) return null;
		
		switch primary {
			case None:
			case Some(p): if(!cache.contains(p)) p.findType(path, Inside, from, 0, cache + this)._andOr(t => {
				if(depth != 0) {
					cache += t;
					depth--;
				} else {
					return t;
				}
			}, {
				cache += p;
			});
		}

		/*for(file in files) if(!cache.contains(file)) {
			//if(path.simpleName().contains("Tail"))trace(from?.name.name, path.simpleName(), file.path);
			file.findType(path, Inside, from, 0, cache)._match(
				at(null) => {},
				at(t!!, when(depth != 0)) => {
					cache += t;
					depth--;
				},
				at(t!!) => {
					//if(path.simpleName().contains("Head")) trace(path.span().display(), t);
					return t;
				}
			);
		}*/

		final res = super.findType(path, search, from, depth, cache);
		//if(path.simpleName() == "LinkIterator")trace(this.path, primary.exists(p -> p == cache.head()), res);
		return res ?? if(search == Inside) {
			null;
		} else {
			var a = outer.findType(path, Outside, from, depth, cache + this);
			//trace(path.simpleName(), path.span().display());
			a;
		}
	}


	override function findCategory(ctx: Ctx, cat: Type, forType: Type, from: AnyTypeDecl, cache: Cache = Nil): Array<Category> {
		if(cache.contains(this)) return [];
		cache += this;
		
		final candidates = switch primary {
			case None: [];
			case Some(p): p.findCategory(ctx, cat, forType, from, cache);
		};

		for(file in files) {
			switch file.findCategory(ctx, cat, forType, from, cache) {
				case []:
				case found: candidates.pushAll(found);
			}
		}

		for(unit in units) {
			switch unit.findCategory(ctx, cat, forType, from, cache) {
				case []:
				case found: candidates.pushAll(found);
			}
		}

		// TODO: figure out a better solution for this
		//if(!this.path.startsWith("C:/Users/alani/Documents/GitHub/star/stdlib")) {
			//trace(cat.fullName(), this.path);
			candidates.pushAll(outer.findCategory(ctx, cat, forType, from, cache));
		//}

		return candidates;
	}
}