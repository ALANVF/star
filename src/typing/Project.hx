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

	override function findType(path: LookupPath, search: Search, from: Null<AnyTypeDecl>, depth = 0, cache: Cache = Nil): Null<Type> {
		//if(search != Inside && cache.contains(this)) return None;

		/*switch main {
			case None:
			case Some(m): if(!cache.contains(m)) switch m.findType(path, Inside, from, 0, cache + this) {
				case None:
				case Some(t) if(depth != 0):
					cache += t;
					depth--;
				case Some(t): return Some(t);
			}
		}*/
		
		return super.findType(path, search, from, depth, cache)._match(
			at(t!) => t,
			_ => if(search != Inside && useStdlib) {
				STDLIB._match(
					at(stdlib!, when(!cache.contains(this))) => {
						cache += this;
						
						final span = path.span();
						
						stdlib.findType(List3.of([span, "Star", []], ...path), Inside, from, 0, Nil)._or(
							stdlib.findType(List3.of([span, "Star", []], [span, "Core", []], ...path), Inside, from, 0, Nil)
						);
					},
					_ => null
				);
			} else {
				null;
			}
		);
	}

	inline function pass1() {
		Pass1.resolveProjectContents(this);
	}
}