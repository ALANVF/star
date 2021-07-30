package compiler.nim;

@:coreType
@:noCompletion
abstract _Type {}

typedef Type = AnyType<_Type>