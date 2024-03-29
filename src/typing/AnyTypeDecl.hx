package typing;

import text.Span;
import errors.Error;
import parsing.ast.Ident;

@:publicFields
@:structInit
abstract class AnyTypeDecl implements ITypeable implements ITypeLookupDecl {
	final errors: Array<Error> = [];
	var span: Span;
	var name: Ident;
	var lookup: ITypeLookup;
	@:optional var thisType: Type;

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


	// Cases

	function allValueCases(): Array<ValueCase> {
		return [];
	}
	
	function allTaggedCases(): Array<TaggedCase> {
		return [];
	}


	// Categories

	function findThisCategory(
		ctx: Ctx,
		cat: Type,
		from: AnyTypeDecl,
		cache: Cache = Nil
	): Array<Category> {
		return findCategory(ctx, cat, thisType, from, cache);
	}
}