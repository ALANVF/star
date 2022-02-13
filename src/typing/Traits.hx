package typing;

import errors.Error;
import text.Span;
import parsing.ast.Stmt;
import parsing.ast.Ident;

typedef VErrors = {
	final errors: Array<Error>;

	function hasErrors(): Bool;
	function allErrors(): Array<Error>;
}

typedef VDecl = VErrors & {
	final span: Span;
	
	function declName(): String;
}

typedef IAnyMethod = VDecl & {
	final decl: AnyTypeDecl;
	var hidden: Null<Option<Type>>;
	var noInherit: Bool;
	var native: Null<Option<Ident>>;
	var isAsm: Bool;
	final body: Null<Array<Stmt>>;
}

typedef IDefaultInit = {
	var defaultInit: Option<DefaultInit>;
}

typedef IDeinit = {
	var deinit: Option<Deinit>;
}

typedef IInits = {
	final inits: Array<Init>;
}

/*typedef ILookupType = {
	function makeTypePath(path: TypePath): Type;
	
	function findType(path: LookupPath, search: Search, from: Null<AnyTypeDecl>, depth: Int = 0, cache: Cache = Nil): Null<Type>;

	function findCategory(ctx: Ctx, cat: Type, forType: Type, from: AnyTypeDecl, cache: Cache = Nil): Array<Category>;
}*/

typedef IMembers = {
	final members: Array<Member>;
}

typedef IMethod = IAnyMethod & {
	var ret: Null<Type>;
	var isMain: Bool;
	var isGetter: Bool;
	var isSetter: Bool;
	var isInline: Bool;
	var isMacro: Bool;
}

typedef IMethods = {
	final methods: Array<Method>;
}

typedef IOperators = {
	final operators: Array<Operator>;
}

typedef IStaticDeinit = {
	var staticDeinit: Option<StaticDeinit>;
}

typedef IStaticMembers = {
	final staticMembers: Array<Member>;
}

typedef IStaticMethods = {
	final staticMethods: Array<StaticMethod>;
}

/*typedef ITaggedCases = ITypeDecl & {
	final taggedCases: Array<TaggedCase>;
}*/

/*typedef ITypeDecl = ILookupType & VDecl & {
	final lookup: ITypeLookup;
	final name: Ident;
	var thisType: Type;

	function fullName(?cache: TypeCache): String;

	function hasParentDecl(decl: TypeDecl): Bool;
	function hasChildDecl(decl: TypeDecl): Bool;

	function hasParentType(type: Type): Bool;
	function hasChildType(type: Type): Bool;

	function canSeeMember(member: Member): Bool;
	function canSeeMethod(method: AnyMethod): Bool;

	function instMembers(from: AnyTypeDecl): Array<Member>;
}*/

/*typedef IFullTypeDecl = ITypeDecl & {
	var params: Array<Type>;
	var thisType: Type;
}*/

typedef ITypeDecls = {
	final decls: Array<TypeDecl>;
}

typedef ITypeVars = {
	final typevars: MultiMap<String, TypeVar>;
}

/*typedef IValueCases = ITypeDecl & {
	final valueCases: Array<ValueCase>;
}*/