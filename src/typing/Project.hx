package typing;

import sys.FileSystem;

class Project extends Dir {
	var main: Option<File> = None;

	static function fromMainPath(path) {
		final absPath = FileSystem.absolutePath(path);

		if(!FileSystem.exists(absPath)) {
			throw 'Directory "$absPath" does not exist!';
		}

		if(!FileSystem.isDirectory(absPath)) {
			throw 'Expected "$absPath" to be a directory, not a file!';
		}

		final root = new Project({name: path.substringAfterLast("/"), path: absPath});
		
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
}