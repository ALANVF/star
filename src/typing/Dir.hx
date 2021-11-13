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
abstract class Dir implements ITypeLookup {
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

	function findType(path: LookupPath, search: Search, from: Null<AnyTypeDecl>, depth = 0, cache: Cache = Nil): Null<Type> {
		if(/*search!=Inside &&*/ cache.contains(this)) {
			return null;
		} else {
			cache += this;
		}
		
		for(file in files) if(!cache.contains(file)) {
			//if(path.simpleName().contains("Tail"))trace(from._and(f => f.name.name), path.simpleName(), file.path);
			file.findType(path, Inside, from, 0, cache)._match(
				at(null) => {},
				at(t!!, when(depth != 0)) => {
					cache += t;
					depth--;
				},
				at(t!!) => return t
			);
		}

		path._match(
			at([[span, name, args]], when(depth == 0)) => {
				for(unit in units) if(!cache.contains(unit)) {
					unit.primary._match(
						at(None) => {},
						at(Some(p)) => p.findType(path, Inside, from, 0, cache + unit)._match(
							at(null) => {},
							at(t!!, when(depth != 0)) => {
								cache += t;
								depth--;
							},
							at(t!!) => if(unit.name == name) {
								return {t: TModular(t, unit), span: span}
							} else {
								return t;
							}
						)
					);
				}
			},
			at([[s, name, args], ...rest]) => {
				for(unit in units) if(!cache.contains(unit) && unit.name == name) {
					//trace(s._and(ss=>ss.display()), name);
					unit.findType(rest, Inside, from, 0, cache)._match(
						at(null) => unit.primary.toNull()._and(p => p.findType(path, Inside, from, 0, cache))._match(
							at(null) => {},
							at(t!!, when(depth != 0)) => {
								cache += t;
								depth--;
							},
							at(t!!) => return t
						),
						at(t!!, when(depth != 0)) => {
							cache += t;
							depth--;
						},
						at(t!!) => return t
					);
				}
			},
			_ => throw "bad"
		);

		return null;
	}

	
	function findCategory(ctx: Ctx, cat: Type, forType: Type, from: AnyTypeDecl, cache: Cache = Nil): Array<Category> {
		if(cache.contains(this)) return [];
		cache += this;
		
		final candidates = [];

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

		return candidates;
	}
}