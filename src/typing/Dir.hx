package typing;

import sys.FileSystem;

using hx.strings.Strings;

@:build(util.Auto.build())
@:autoBuild(util.Auto.build())
abstract class Dir {
	final path: String;
	final units: Array<Unit> = [];
	final files: Array<File> = [];

	function buildUnits() {
		final entries = FileSystem.readDirectory(path);
		final names = [];

		for(entry in entries) {
			entry = entry.removeTrailing(".star");

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
				path: '$path/$dirName',
				outer: this
			});

			if(FileSystem.exists(filePath)) {
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
			} else {
				this.addNestedName(path, name);
			}
		} else {
			final unitPath = '$path/$name';
			final filePath = '$unitPath.star';

			if(FileSystem.exists(unitPath)) {
				final unit = new Unit({
					path: unitPath,
					outer: this
				});

				if(FileSystem.exists(filePath)) {
					unit.primary = Some(new File(this, filePath, unit));
				}

				unit.buildUnits();
				units.push(unit);
			} else if(FileSystem.exists(filePath)) {
				files.push(new File(this, filePath));
			} else {
				throw "wtf";
			}
		}
	}

	function gatherFiles(gather: Array<File>) {
		gather.pushAll(files);
		
		for(unit in units) {
			unit.gatherFiles(gather);
		}
	}
}