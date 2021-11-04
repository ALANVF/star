package compiler;

@:publicFields
class TypePathTools {
	@:noUsing
	static function getFullPath(cmp: Compiler, lookup: typing.ITypeLookup): TypePath {
		return lookup._match(
			at(file is typing.File) => {
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
			},
			at(type is typing.TypeDecl) => {
				getFullPath(cmp, type.lookup).concat([{name: type.name.name, args: switch type.params.map(p -> Type.fromType(cmp, p)) {
					case []: None;
					case params: Some(params);
				}}]);
			},
			_ => throw "error!"
		);
	}
	
	static function form(path: TypePath) {
		return path.map(p -> switch p {
			case {name: n, args: None}: n;
			case {name: n, args: Some(args)}: '$n<' + args.map(a -> a.form()).join(", ") + ">";
		}).join("::");
	}
}