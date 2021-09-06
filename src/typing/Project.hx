package typing;

import sys.FileSystem;

class Project extends Dir {
	static var STDLIB: Null<Project> = null;

	var main: Option<File> = None;
	var useStdlib: Bool;

	static function fromMainPath(path, useStdlib = true) {
		final absPath = FileSystem.absolutePath(path);

		if(!FileSystem.exists(absPath)) {
			throw 'Directory "$absPath" does not exist!';
		}

		if(!FileSystem.isDirectory(absPath)) {
			throw 'Expected "$absPath" to be a directory, not a file!';
		}

		final root = new Project({
			name: path.substringAfterLast("/"),
			path: absPath,
			useStdlib: useStdlib
		});
		
		root.buildUnits();

		return root;
	}

	override function gatherFiles(gather: Array<File>) {
		main.forEach(m -> gather.push(m));
		super.gatherFiles(gather);
	}

	function allFiles() {
		final files = [];
		
		this.gatherFiles(files);

		return files;
	}

	override function findType(path: LookupPath, absolute = false, cache: List<{}> = Nil): Option<Type> {
		switch super.findType(path, absolute, cache) {
			case t = Some(_): return t;
			case None: if(absolute && useStdlib) STDLIB._match(
				at(stdlib!, when(!cache.contains(this))) => {
					cache = cache.prepend(this);

					return STDLIB.findType(List3.of([null, "Star", []]), false, cache).value()
						.findType(path, false, cache);
				},
				_ => return None
			); else {
				return None;
			}
		}
	}

	inline function pass1() {
		Pass1.resolveProjectContents(this);
	}
}