package typing;

class Unit extends Dir {
	final outer: Dir;
	var primary: Option<File> = None;
	//var primaryType: Option<Type> = None;

	override function gatherFiles(gather: Array<File>) {
		primary.forEach(p -> gather.push(p));
		super.gatherFiles(gather);
	}

	override function findType(path: LookupPath, absolute = false, cache: List<{}> = Nil): Option<Type> {
		if(absolute) {
			if(cache.contains(this)) {
				return None;
			} else {
				cache = cache.prepend(this);
			}
		}
		
		return path._match(
			at([[span, typeName, _]], when(typeName == name)) => switch primary.flatMap(p -> p.findType(path, false, cache)) {
				case Some(type): Some({t: TModular(type, this), span: span});
				case None:
					super.findType(path, false, cache).orDo(
						if(absolute) outer.findType(path, true, cache) else None
					);
			},
			at([[span, typeName, _], ...rest], when(typeName == name)) => switch primary.flatMap(p -> p.findType(path, false, cache)) {
				case Some(type): Some({t: TModular(type, this), span: span});
				// lol is this dead code?
				case None: switch primary.flatMap(p -> p.findType(path, false, cache)) {
					case Some(type): Some(type);
					case None:
						super.findType(rest, false, cache.prepend(this)).orDo(
							outer.findType(path, true, cache.prepend(this))
						);
				}
			},
			_ => {
				switch primary.flatMap(p -> p.findType(path, false, cache)) {
					case Some(type) if(cache.contains(type)):
						return outer.findType(path, true, cache.prepend(this));
					case Some(type): Some({t: TModular(type, this)});
					case None:
						super.findType(path, false, cache.prepend(this)).orDo(
							outer.findType(path, true, cache.prepend(this))
						);
				}
			}
		);
	}
}