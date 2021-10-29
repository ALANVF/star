package typing;

import reporting.Diagnostic;
import text.Span;
import parsing.ast.Stmt;
import parsing.ast.Ident;

typedef VErrors = {
	final errors: Array<Diagnostic>;

	function hasErrors(): Bool;
	function allErrors(): Array<Diagnostic>;
}

typedef IDecl = VErrors & {
	final span: Span;
	
	function declName(): String;
}

typedef IAnyMethod = IDecl & {
	final decl: ITypeDecl;
	var hidden: Option<Option<Type>>;
	var noInherit: Bool;
	var native: Option<Option<Ident>>;
	var isAsm: Bool;
	final body: Option<Array<Stmt>>;
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

typedef ILookupType = {
	function makeTypePath(path: TypePath): Type;
	
	function findType(path: LookupPath, search: Search, from: Null<ITypeDecl>, ?depth: Int, ?cache: List<{}>): Option<Type>;

	function findCategory(cat: Type, forType: Type, from: ITypeDecl, ?cache: List<{}>): Array<Category>;
}

typedef IMembers = {
	final members: Array<Member>;
}

typedef IMethod = IAnyMethod & {
	var ret: Option<Type>;
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

typedef ITaggedCases = ITypeDecl & {
	final taggedCases: Array<TaggedCase>;
}

typedef ITypeDecl = ILookupType & IDecl & {
	final lookup: ILookupType;
	final name: Ident;
	var thisType: Type;

	function fullName(?cache: List<Type>): String;

	function hasParentDecl(decl: TypeDecl): Bool;
	function hasChildDecl(decl: TypeDecl): Bool;

	function hasParentType(type: Type): Bool;
	function hasChildType(type: Type): Bool;

	function canSeeMember(member: Member): Bool;
	function canSeeMethod(method: AnyMethod): Bool;

	function instMembers(from: ITypeDecl): Array<Member>;
}

typedef IFullTypeDecl = ITypeDecl & {
	var params: Array<Type>;
	var thisType: Type;
}

typedef ITypeDecls = {
	final decls: Array<TypeDecl>;
}

typedef ITypeVars = {
	final typevars: MultiMap<String, TypeVar>;
}

typedef IValueCases = ITypeDecl & {
	final valueCases: Array<ValueCase>;
}