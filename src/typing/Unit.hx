package typing;

class Unit extends Dir {
	final outer: Dir;
	var primary: Option<File> = None;
	//var primaryType: Option<Type> = None;

	override function gatherFiles(gather: Array<File>) {
		primary.forEach(p -> gather.push(p));
		super.gatherFiles(gather);
	}

	override function findType(path: LookupPath, absolute = false, cache: List<{}> = Nil) {
		if(absolute) {
			if(cache.contains(this)) {
				return None;
			} else {
				cache = cache.prepend(this);
			}
		}

		return path._match(
			at([[typeName, _]], when(typeName == name)) => switch primary.flatMap(p -> p.findType(path, false, cache)) {
				case Some(type): Some(new Type(TModular(type, this)));
				case None: None;
			},
			at([[typeName, _], ...rest], when(typeName == name)) => switch primary.flatMap(p -> p.findType(path, false, cache)) {
				case Some(type): Some(new Type(TModular(type, this)));
				case None: switch primary.flatMap(p -> p.findType(rest, false, cache)) {
					case Some(type): Some(type);
					case None:
						for(unit in units) {
							switch unit.findType(rest, false, cache) {
								case Some(type): return Some(type);
								case None:
							}
						}
						for(file in files) {
							switch file.findType(rest, false, cache) {
								case Some(type): return Some(type);
								case None:
							}
						}
						None;
				}
			},
			_ => {
				switch primary.flatMap(p -> p.findType(path, false, cache)) {
					case Some(type): Some(new Type(TModular(type, this)));
					case None:
						for(unit in units) {
							switch unit.findType(path, false, cache) {
								case Some(type): return Some(type);
								case None:
							}
						}
		
						for(file in files) {
							switch file.findType(path, false, cache) {
								case Some(type): return Some(type);
								case None:
							}
						}
		
						if(absolute) outer.findType(path, true, cache) else None;
				}
			}
		);
	}
}