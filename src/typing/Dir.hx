package typing;

import sys.FileSystem;

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
			final fileName = '$dirName.star';
			final filePath = '$path/$fileName';
			final unit = new Unit({
				name: toName(dirName),
				path: '$path/$dirName',
				outer: this
			});

			if(FileSystem.exists(filePath) && !FileSystem.isDirectory(filePath)) {
				unit.primary = Some(new File(this, filePath, unit));
				fileNames.remove(fileName);
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
				final unit = new Unit({
					name: toName(name),
					path: unitPath,
					outer: this
				});

				if(FileSystem.exists(filePath) && !FileSystem.isDirectory(filePath)) {
					unit.primary = Some(new File(this, filePath, unit));
				}

				unit.buildUnits();
				units.push(unit);
			} else if(FileSystem.exists(filePath) && !FileSystem.isDirectory(filePath)) {
				files.push(new File(this, filePath));
			}
		}
	}

	function gatherFiles(gather: Array<File>) {
		gather.pushAll(files);
		
		for(unit in units) {
			unit.gatherFiles(gather);
		}
	}

	function findType(path: LookupPath, absolute = false, cache: List<{}> = Nil) {
		if(absolute) {
			if(cache.contains(this)) {
				return None;
			} else {
				cache = cache.prepend(this);
			}
		}
		
		for(file in files) {
			switch file.findType(path, absolute, cache) {
				case t = Some(_): return t;
				case None:
			}
		}

		for(unit in units) {
			switch unit.findType(path, absolute, cache) {
				case t = Some(_): return t;
				case None:
			}
		}

		return None;
	}
}