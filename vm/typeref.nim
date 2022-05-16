import util
import ids
import std/tables

{.experimental: "notNil".}

public:
 type
    KTypeRef = enum trDecl, trInst, trTypeVar, trThis
    TypeInstCtx = Table[TypeVarID, TypeRef]
    TypeRef {.shallow.} = object
        case kind: KTypeRef
        of trDecl:
            declID: TypeID
        of trInst:
            instID: TypeID
            instCtx: TypeInstCtx
        of trTypeVar:
            tvar: TypeVar
        of trThis: nil
    
    TypeVarInstCtx = TableRef[TypeVar, TypeRef] not nil

func `==`*(t1, t2: TypeRef): bool =
    t1.kind == t2.kind and (
        case t1.kind
        of trDecl: t1.declID == t2.declID
        of trInst: t1.instID == t2.instID and t1.instCtx == t2.instCtx
        of trTypeVar: t1.tvar == t2.tvar
        of trThis: true
    )