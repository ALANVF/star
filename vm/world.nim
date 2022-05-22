import util
import ids
import typeref
import values
import decls
import std/tables

public:
 type
    Version = (uint8, uint8, uint8)
    World = ref object
        typeDecls: seq[BaseDecl]
        entrypoint: (TypeID, MethodID)

        version: Version

        staticFields: Table[TypeRef, ref seq[Value]]

        ptrTypeCache: Table[TypeRef, TypeRef]
        
        defaultValue, defaultMultiKind,
            defaultVoid, defaultBool,
            defaultInt8, defaultInt16, defaultInt32, defaultInt64,
            defaultUInt8, defaultUInt16, defaultUInt32, defaultUInt64,
            defaultDec32, defaultDec64,
            defaultChar,
            defaultStr,
            defaultPtr,
            defaultIterable1, defaultIterable2,
            defaultIterator1, defaultIterator2,
            defaultFunc0, defaultFunc1, defaultFunc2, defaultFunc3: TypeID
        
        defaultVoidRef, defaultBoolRef,
            defaultInt8Ref, defaultInt16Ref, defaultInt32Ref, defaultInt64Ref,
            defaultUInt8Ref, defaultUInt16Ref, defaultUInt32Ref, defaultUInt64Ref,
            defaultDec32Ref, defaultDec64Ref,
            defaultCharRef,
            defaultStrRef: TypeRef

proc makePtrTo*(world: World, t: TypeRef): TypeRef =
    if world.ptrTypeCache.contains(t):
        return world.ptrTypeCache[t]
    else:
        let pt = TypeRef(kind: trInst, instID: world.defaultPtr, instCtx: {1.TypeVarID: t}.toTable)
        world.ptrTypeCache[t] = pt
        return pt