package typing;

import text.Span;
import reporting.Diagnostic;
import parsing.ast.Ident;

@:build(util.Auto.build())
abstract class AnyTypeDecl implements ITypeable implements ITypeLookupDecl {
	final errors: Array<Diagnostic> = [];
	final span: Span;
	final name: Ident;
	final lookup: ITypeLookup;
	@ignore var thisType: Type;

	abstract function declName(): String;
	
	function findType(path: LookupPath, search: Search, from: Null<AnyTypeDecl>, depth: Int = 0, cache: Cache = Nil): Null<Type> {
		/* stupid fix for HL. it generates
		 *     (...,...,...,...,i32,...):...
		 * instead of 
		 *     (...,...,...,...,ref(i32),...):...
		*/
		@:keep depth;
		throw null;
	}

	function findThisCategory(
		ctx: Ctx,
		cat: Type,
		from: AnyTypeDecl,
		cache: Cache = Nil
	): Array<Category> {
		return findCategory(ctx, cat, thisType, from, cache);
	}
}