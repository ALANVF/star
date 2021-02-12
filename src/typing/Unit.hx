package typing;

class Unit extends Dir {
	final outer: Dir;
	var primary: Option<File> = None;

	override function gatherFiles(gather: Array<File>) {
		primary.forEach(p -> gather.push(p));
		super.gatherFiles(gather);
	}
}