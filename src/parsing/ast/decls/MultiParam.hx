package parsing.ast.decls;

typedef MultiParam = {label: Option<Ident>, name: Option<Ident>, type: Type, value: Option<Expr>};