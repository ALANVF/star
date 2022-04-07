package codegen;

enum TypeRef {
	TDecl(id: TypeID);
	TInst(id: TypeID, inst: TypeInstCtx);
	TTypeVar(id: TVar);
	TThis;
}