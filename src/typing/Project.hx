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

	override function findType(path: LookupPath, search: Search, from: Null<Traits.ITypeDecl>, depth = 0, cache: List<{}> = Nil): Option<Type> {
		//if(search != Inside && cache.contains(this)) return None;

		/*switch main {
			case None:
			case Some(m): if(!cache.contains(m)) switch m.findType(path, Inside, from, 0, cache.prepend(this)) {
				case None:
				case Some(t) if(depth != 0):
					cache = cache.prepend(t);
					depth--;
				case Some(t): return Some(t);
			}
		}*/
		
		return switch super.findType(path, search, from, depth, cache) {
			case Some(t): Some(t);
			case None if(search != Inside && useStdlib): STDLIB._match(
				at(stdlib!, when(!cache.contains(this))) => {
					cache = cache.prepend(this);
					
					final span = path.span();
					
					stdlib.findType(List3.of([span, "Star", []], ...path), Inside, from, 0, Nil).orDo(
						stdlib.findType(List3.of([span, "Star", []], [span, "Core", []], ...path), Inside, from, 0, Nil)
					);
				},
				_ => None
			);
			case None: None;
		};
	}

	inline function pass1() {
		Pass1.resolveProjectContents(this);
	}
}