package typing;

enum Message<T> {
	Single(?category: Type, name: String);
	Multi(?category: Type, labels: Array<String>, exprs: TExprs);
	Cast(?category: Type, type: Type): Message<TExpr>;

	//CSingleStatic(method: SingleStaticMethod): Message<Type>;
	/*CSingleInit(init: SingleInit): Message<Type>;
	CSingleCase(tcase: SingleTaggedCase): Message<Type>;*/
	//CSingle(method: SingleMethod): Message<TExpr>;
}