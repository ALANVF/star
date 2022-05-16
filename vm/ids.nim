import util

public:
 type
    TypeID = uint32
    TypeVarID = uint16
    MethodID = uint32
    InitID = uint32
    MemberID = uint16
    KindTag = uint16

public:
 type
    LocalID = uint16
    FieldID = uint16
    LoopID = uint8

public:
 type
    KTypeVar = enum tvDecl, tvMethod
    TypeVar = object
        kind: KTypeVar
        id: TypeVarID