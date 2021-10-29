package typing;

import sys.FileSystem;
import typing.Traits;

function toName(name: String) {
	if(name.contains("+")) {
		var category = name.substringAfter("+");
		if(category.contains("[")) {
			category = category.substringBefore("[");
		}
		return category;
	} else if(name.startsWith("(") && name.endsWith(")")) {
		var protocol = name.substr(1, name.length - 2);
		if(protocol.contains("[")) {
			protocol = protocol.substringBefore("[");
		}
		return protocol;
	} else if(name.endsWith("]")) {
		return name.substringBefore("[");
	} else {
		return name;
	}
}

@:build(util.Auto.build())
@:autoBuild(util.Auto.build())
abstract class Dir {
	final name: String;
	final path: String;
	final units: Array<Unit> = [];
	final files: Array<File> = [];

	function buildUnits() {
		final entries = FileSystem.readDirectory(path);
		final names = [];

		for(entry in entries) {
			if(entry.endsWith(".star")) {
				entry = entry.removeTrailing(".star");
			} else if(!FileSystem.isDirectory('$path/$entry')) {
				continue;
			}

			if(!names.contains(entry)) {
				names.push(entry);
			}
		}

		for(name in names) {
			this.addName(name);
		}

		/*this._match(
			at(u is Unit) => if(name == "List") {
				trace(path);
				trace(name);
				trace(u.primary.map(p -> p.path));
				trace(u.primary.map(p -> p.unit.value() == u));
				trace(units);
				trace("files:");
				for(f in files) trace(f.path);
			} else if(name == "Linked") {
				trace(path);
				trace(name);
				trace(u.primary.map(p -> p.path));
				trace(units);
				trace("files:");
				for(f in files) trace(f.path);
			},
			_ => {}
		);*/
	}

	function addNestedName(outer: String, name: String) {
		final path = '$outer/$name';
		final entries = FileSystem.readDirectory(path);
		final fileNames = [];
		final dirNames = [];

		for(entry in entries) {
			if(entry.endsWith(".star")) {
				fileNames.push(entry);
			} else if(!FileSystem.isDirectory('$path/$entry')) {
				continue; // Some it's some other file
			} else if(entry.charCodeAt8(0).isLowerCase()) {
				this.addNestedName(path, entry);
			} else {
				dirNames.push(entry);
			}
		}

		for(dirName in dirNames) {
			final unitName = toName(dirName);
			final unit = new Unit({
				name: unitName,
				path: '$path/$dirName',
				outer: this
			});
			final fileName = '$dirName.star';
			final filePath = '$path/$fileName';

			if(FileSystem.exists(filePath) && !FileSystem.isDirectory(filePath)) {
				unit.primary = Some(new File(this, filePath, unit));
				fileNames.remove(fileName);
			} else {
				fileNames.find(n -> toName(n.substringBeforeLast(".star")) == unitName)._match(
					at(fileName2!) => {
						final filePath2 = '$path/$fileName2';

						unit.primary = Some(new File(this, filePath2, unit));
						fileNames.remove(fileName2);
					},
					_ => {}
				);
			}
			
			unit.buildUnits();
			units.push(unit);
		}

		for(fileName in fileNames) {
			files.push(new File(this, '$path/$fileName'));
		}
	}

	function addName(name: String) {
		if(name.charCodeAt8(0).isLowerCase()) {
			if(FileSystem.exists('$path/$name.star')) {
				throw 'Invalid file "$name" in directory "$path"';
			} else if(FileSystem.isDirectory('$path/$name')) {
				this.addNestedName(path, name);
			}
		} else {
			final unitPath = '$path/$name';
			final filePath = '$unitPath.star';

			if(FileSystem.exists(unitPath) && FileSystem.isDirectory(unitPath)) {
				final unitName = toName(name);
				final unit = new Unit({
					name: unitName,
					path: unitPath,
					outer: this
				});

				if(FileSystem.exists(filePath) && !FileSystem.isDirectory(filePath)) {
					unit.primary = Some(new File(this, filePath, unit));
				} else {
					FileSystem.readDirectory(path)
					.filter(n -> !FileSystem.isDirectory('$path/$n') && n.endsWith(".star"))
					.find(n -> toName(n.substringBeforeLast(".star")) == unitName)
					._match(
						at(fileName!) => {
							unit.primary = Some(new File(this, '$path/$fileName', unit));
						},
						_ => {}
					);
				}

				unit.buildUnits();
				units.push(unit);
			} else if(FileSystem.exists(filePath) && !FileSystem.isDirectory(filePath)) {
				// don't add files that are already part of a unit
				if(!units.some(u -> u.primary.exists(p -> p.path == filePath))) {
					files.push(new File(this, filePath));
				}
			}
		}
	}

	function gatherFiles(gather: Array<File>) {
		gather.pushAll(files);
		
		for(unit in units) {
			unit.gatherFiles(gather);
		}
	}

	function makeTypePath(path: TypePath) {
		return path.toType(this);
	}

	function findType(path: LookupPath, search: Search, from: Null<Traits.ITypeDecl>, depth = 0, cache: List<{}> = Nil): Option<Type> {
		if(/*search!=Inside &&*/ cache.contains(this)) {
			return None;
		} else {
			cache = cache.prepend(this);
		}
		
		for(file in files) if(!cache.contains(file)) {
			//if(path.simpleName().contains("Tail"))trace(from._and(f => f.name.name), path.simpleName(), file.path);
			switch file.findType(path, Inside, from, 0, cache) {
				case None:
				case Some(t) if(depth != 0):
					cache = cache.prepend(t);
					depth--;
				case Some(t):
					//if(path.simpleName().contains("Tail")) trace(path.span().display());
					return Some(t);
			}
		}

		path._match(
			at([[span, name, args]], when(depth == 0)) => {
				for(unit in units) if(!cache.contains(unit) && unit.name == name) {
					unit.primary._match(
						at(None) => {},
						at(Some(p)) => switch p.findType(path, Inside, from, 0, cache.prepend(unit)) {
							case None:
							case Some(t) if(depth != 0):
								cache = cache.prepend(t);
								depth--;
							case Some(t): return Some({t: TModular(t, unit), span: span});
						}
					);
				}
			},
			at([[s, name, args], ...rest]) => {
				for(unit in units) if(!cache.contains(unit) && unit.name == name) {
					//trace(s._and(ss=>ss.display()), name);
					switch unit.findType(rest, Inside, from, 0, cache) {
						case None: switch unit.primary.flatMap(p -> p.findType(path, Inside, from, 0, cache)) {
							case None:
							case Some(t) if(depth != 0):
								cache = cache.prepend(t);
								depth--;
							case Some(t): return Some(t);
						}
						case Some(t) if(depth != 0):
							cache = cache.prepend(t);
							depth--;
						case Some(t): return Some(t);
					}
				}
			},
			_ => throw "bad"
		);

		return None;
	}

	
	function findCategory(cat: Type, forType: Type, from: ITypeDecl, cache: List<{}> = Nil): Array<Category> {
		if(cache.contains(this)) return [];
		
		final candidates = [];

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