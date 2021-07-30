package compiler.nim;

@:coreType
@:noCompletion
abstract _TypeDecl {}

typedef TypeDecl = AnyType<_TypeDecl>