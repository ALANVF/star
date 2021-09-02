package typing;

import reporting.Diagnostic;
import text.Span;
import parsing.ast.Stmt;
import parsing.ast.Ident;

typedef IErrors = {
	final errors: Array<Diagnostic>;

	function hasErrors(): Bool;
	function allErrors(): Array<Diagnostic>;
}

typedef IDecl = IErrors & {
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
	
	function findType(vpath: LookupPath, ?absolute: Bool, ?cache: List<{}>): Option<Type>;
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

typedef IParents = {
	final parents: Array<Type>;
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
}

typedef ITypeDecls = {
	final decls: Array<TypeDecl>;
}

typedef IValueCases = ITypeDecl & {
	final valueCases: Array<ValueCase>;
}