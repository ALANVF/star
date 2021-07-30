package compiler.nim;

typedef Elif<T> = {cond: Expr, body: Array<T>}