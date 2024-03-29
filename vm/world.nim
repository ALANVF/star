import util
import ids
import typeref
import values
import decls
import std/tables
import std/sets
import std/streams

public:
 type
    Version = (uint8, uint8, uint8)
    World = ref object
        typeDecls: seq[BaseDecl]
        entrypoint: (TypeID, MethodID)

        version: Version

        staticFields: Table[TypeID, ref seq[Value]]

        ptrTypeCache: Table[TypeRef, TypeRef]
        
        defaultValue, defaultMultiKind,
            defaultVoid, defaultBool,
            defaultInt8, defaultInt16, defaultInt32, defaultInt64,
            defaultUInt8, defaultUInt16, defaultUInt32, defaultUInt64,
            defaultFloat32, defaultFloat64, defaultDec64,
            defaultChar,
            defaultStr,
            defaultPtr, defaultVoidPtr,
            defaultIterable1, defaultIterable2,
            defaultIterator1, defaultIterator2,
            defaultFunc0, defaultFunc1, defaultFunc2, defaultFunc3: TypeID
        
        defaultVoidRef, defaultBoolRef,
            defaultInt8Ref, defaultInt16Ref, defaultInt32Ref, defaultInt64Ref,
            defaultUInt8Ref, defaultUInt16Ref, defaultUInt32Ref, defaultUInt64Ref,
            defaultFloat32Ref, defaultFloat64Ref, defaultDec64Ref,
            defaultCharRef,
            defaultStrRef,
            defaultVoidPtrRef: TypeRef
        
        fileStreams: HashSet[FileStream]

proc makePtrTo*(world: World, t: TypeRef): TypeRef =
    if world.ptrTypeCache.contains(t):
        return world.ptrTypeCache[t]
    else:
        let pt = TypeRef(kind: trInst, instID: world.defaultPtr, instCtx: {1.TypeVarID: t}.toTable)
        world.ptrTypeCache[t] = pt
        return pt

proc getOrInitStaticFields*(world: World, decl: BaseDecl): ref seq[Value] =
    let declID = decl.id
    if world.staticFields.contains(declID):
        world.staticFields[declID]
    else:
        let members = decl.staticMembers
        if members == nil:
            nil
        else:
            let numMembers = members[].len
            if numMembers == 0:
                nil
            else:
                let fields = new seq[Value]
                newSeq(fields[], numMembers)
                world.staticFields[declID] = fields
                fields