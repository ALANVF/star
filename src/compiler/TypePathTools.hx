package compiler;

@:publicFields
class TypePathTools {
	@:noUsing
	static function getFullPath(cmp: Compiler, lookup: typing.ILookupType): TypePath {
		return if(lookup is typing.File) {
			var file = cast(lookup, typing.File);
			if(file.dir is typing.Unit) {
				var dir = file.dir;
				var names = Nil;
	
				while(dir is typing.Unit) {
					final unit = cast(dir, typing.Unit);
	
					names = names.prepend({name: unit.name, args: None});
					dir = unit.outer;
				}
	
				names.toArray();
			} else {
				[];
			}
		} else if(lookup is typing.TypeDecl) {
			final type = cast(lookup, typing.TypeDecl);
			getFullPath(cmp, type.lookup).concat([{name: type.name.name, args: type.params.map(params -> params.map(p -> Type.fromType(cmp, p)))}]);
		} else {
			throw "error!";
		}
	}
	
	static function form(path: TypePath) {
		return path.map(p -> switch p {
			case {name: n, args: None}: n;
			case {name: n, args: Some(args)}: '$n<' + args.map(a -> a.form()).join(", ") + ">";
		}).join("::");
	}
}