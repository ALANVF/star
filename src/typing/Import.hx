package typing;

@:build(util.Auto.build())
class Import {
	final imports: Array<TypePath>;
	final from: Option<TypePath>;
}