import util
import ids
import typeref
import opcode
#import values
import typevar
import methods
import macros
import std/tables

{.experimental: "notNil".}

public:
 type
    BaseDecl = ref object of RootObj
        id: TypeID
        name: string
        typevars: TypeVars

        staticSingleMethods: Table[MethodID, SingleMethod]
        staticMultiMethods: Table[MethodID, MultiMethod]
    
    CategoryDecl = ref object of BaseDecl
        pathType, forType: TypeRef

        staticMembers: Table[MemberID, Member]

        staticInit: nil Opcodes

        singleInits: Table[InitID, SingleInit]
        multiInits: Table[InitID, MultiInit]

        instSingleMethods: Table[MethodID, SingleMethod]
        instMultiMethods: Table[MethodID, MultiMethod]
        instCastMethods: Table[MethodID, CastMethod]
        binaryMethods: Table[MethodID, BinaryMethod]
        unaryMethods: Table[MethodID, UnaryMethod]

    OpaqueDecl = ref object of BaseDecl
        instSingleMethods: Table[MethodID, SingleMethod]
        instMultiMethods: Table[MethodID, MultiMethod]
        instCastMethods: Table[MethodID, CastMethod]
        binaryMethods: Table[MethodID, BinaryMethod]
        unaryMethods: Table[MethodID, UnaryMethod]

    NewtypeDecl = ref object of BaseDecl
        base: TypeRef
        noInherit: bool
        
        staticMembers: Table[MemberID, Member]
        #instMembers: Table[MemberID, Member]

        staticInit: nil Opcodes
        
        instSingleMethods: Table[MethodID, SingleMethod]
        instMultiMethods: Table[MethodID, MultiMethod]
        instCastMethods: Table[MethodID, CastMethod]
        binaryMethods: Table[MethodID, BinaryMethod]
        unaryMethods: Table[MethodID, UnaryMethod]
    
    ModuleDecl = ref object of BaseDecl
        parents: seq[TypeRef]
        case isMain: bool
        of true: main: MethodID
        of false: nil

        staticMembers: Table[MemberID, Member]

        staticInit: nil Opcodes
    
    ClassDecl = ref object of BaseDecl
        parents: seq[TypeRef]
        
        staticMembers: Table[MemberID, Member]
        instMembers: Table[MemberID, Member]

        staticInit: nil Opcodes
        defaultInit: nil Opcodes

        singleInits: Table[InitID, SingleInit]
        multiInits: Table[InitID, MultiInit]
        
        instSingleMethods: Table[MethodID, SingleMethod]
        instMultiMethods: Table[MethodID, MultiMethod]
        instCastMethods: Table[MethodID, CastMethod]
        binaryMethods: Table[MethodID, BinaryMethod]
        unaryMethods: Table[MethodID, UnaryMethod]

        instSingleMethodVTable: Table[TypeID, Table[MethodID, SingleMethod]]
        instMultiMethodVTable: Table[TypeID, Table[MethodID, MultiMethod]]
        instCastMethodVTable: Table[TypeID, Table[MethodID, CastMethod]]
        binaryMethodVTable: Table[TypeID, Table[MethodID, BinaryMethod]]
        unaryMethodVTable: Table[TypeID, Table[MethodID, UnaryMethod]]

        deinit: nil Opcodes
        staticDeinit: nil Opcodes
    
    ProtocolDecl = ref object of BaseDecl
        parents: seq[TypeRef]
        
        staticMembers: Table[MemberID, Member]
        instMembers: Table[MemberID, Member]

        staticInit: nil Opcodes
        defaultInit: nil Opcodes

        singleInits: Table[InitID, SingleInit]
        multiInits: Table[InitID, MultiInit]
        
        instSingleMethods: Table[MethodID, SingleMethod]
        instMultiMethods: Table[MethodID, MultiMethod]
        instCastMethods: Table[MethodID, CastMethod]
        binaryMethods: Table[MethodID, BinaryMethod]
        unaryMethods: Table[MethodID, UnaryMethod]

        instSingleMethodVTable: Table[TypeID, Table[MethodID, SingleMethod]]
        instMultiMethodVTable: Table[TypeID, Table[MethodID, MultiMethod]]
        instCastMethodVTable: Table[TypeID, Table[MethodID, CastMethod]]
        binaryMethodVTable: Table[TypeID, Table[MethodID, BinaryMethod]]
        unaryMethodVTable: Table[TypeID, Table[MethodID, UnaryMethod]]

        deinit: nil Opcodes
        staticDeinit: nil Opcodes
    
    TaggedKindDecl = ref object of BaseDecl
        parents: seq[TypeRef]
        isFlags: bool
        
        staticMembers: Table[MemberID, Member]
        instMembers: Table[MemberID, Member]

        cases: Table[KindTag, TaggedCase]

        staticInit: nil Opcodes
        defaultInit: nil Opcodes
        
        instSingleMethods: Table[MethodID, SingleMethod]
        instMultiMethods: Table[MethodID, MultiMethod]
        instCastMethods: Table[MethodID, CastMethod]
        binaryMethods: Table[MethodID, BinaryMethod]
        unaryMethods: Table[MethodID, UnaryMethod]

        instSingleMethodVTable: Table[TypeID, Table[MethodID, SingleMethod]]
        instMultiMethodVTable: Table[TypeID, Table[MethodID, MultiMethod]]
        instCastMethodVTable: Table[TypeID, Table[MethodID, CastMethod]]
        binaryMethodVTable: Table[TypeID, Table[MethodID, BinaryMethod]]
        unaryMethodVTable: Table[TypeID, Table[MethodID, UnaryMethod]]

        deinit: nil Opcodes
        staticDeinit: nil Opcodes
    
    ValueKindDecl = ref object of BaseDecl
        parents: seq[TypeRef]
        isFlags: bool
        base: ref TypeRef
        
        staticMembers: Table[MemberID, Member]

        cases: Table[KindTag, ValueCase]

        staticInit: nil Opcodes
        
        instSingleMethods: Table[MethodID, SingleMethod]
        instMultiMethods: Table[MethodID, MultiMethod]
        instCastMethods: Table[MethodID, CastMethod]
        binaryMethods: Table[MethodID, BinaryMethod]
        unaryMethods: Table[MethodID, UnaryMethod]

        instSingleMethodVTable: Table[TypeID, Table[MethodID, SingleMethod]]
        instMultiMethodVTable: Table[TypeID, Table[MethodID, MultiMethod]]
        instCastMethodVTable: Table[TypeID, Table[MethodID, CastMethod]]
        binaryMethodVTable: Table[TypeID, Table[MethodID, BinaryMethod]]
        unaryMethodVTable: Table[TypeID, Table[MethodID, UnaryMethod]]

        deinit: nil Opcodes
        staticDeinit: nil Opcodes
    
    Member = ref object
        id: MemberID
        `type`: TypeRef
    
    TaggedCase = ref object
        id: KindTag
        name: string
        slots: ref seq[TypeRef]
        defaultInit: nil Opcodes
    
    ValueCase = ref object
        id: KindTag
        name: string
        valueInit: nil Opcodes


macro genAccessor(name: untyped, types: untyped, ret: untyped): untyped =
    let decl = newIdentNode("decl")
    let first = quote:
        method `name`*(`decl`: BaseDecl): `ret` {.base.} =
            return nil
    
    let res = newStmtList(first)
    
    if ret.kind == nnkPtrTy:
        for node in types:
            let m = quote:
                method `name`*(`decl`: `node`): `ret` =
                    return addr `decl`.`name`
            res.add(m)
    else:
        for node in types:
            let m = quote:
                method `name`*(`decl`: `node`): `ret` =
                    return `decl`.`name`
            res.add(m)
    
    return res

# TODO: figure out better accessors for NewtypeDecl

genAccessor staticMembers, (CategoryDecl, NewtypeDecl, ModuleDecl, ClassDecl, ProtocolDecl, TaggedKindDecl, ValueKindDecl), ptr Table[MemberID, Member]

genAccessor instMembers, (ClassDecl, ProtocolDecl, TaggedKindDecl), ptr Table[MemberID, Member]

genAccessor singleInits, (CategoryDecl, ClassDecl, ProtocolDecl), ptr Table[InitID, SingleInit]
genAccessor multiInits, (CategoryDecl, ClassDecl, ProtocolDecl), ptr Table[InitID, MultiInit]

genAccessor instSingleMethods, (CategoryDecl, OpaqueDecl, NewtypeDecl, ClassDecl, ProtocolDecl, TaggedKindDecl, ValueKindDecl), ptr Table[MethodID, SingleMethod]
genAccessor instMultiMethods, (CategoryDecl, OpaqueDecl, NewtypeDecl, ClassDecl, ProtocolDecl, TaggedKindDecl, ValueKindDecl), ptr Table[MethodID, MultiMethod]
genAccessor instCastMethods, (CategoryDecl, OpaqueDecl, NewtypeDecl, ClassDecl, ProtocolDecl, TaggedKindDecl, ValueKindDecl), ptr Table[MethodID, CastMethod]

genAccessor binaryMethods, (CategoryDecl, OpaqueDecl, NewtypeDecl, ClassDecl, ProtocolDecl, TaggedKindDecl, ValueKindDecl), ptr Table[MethodID, BinaryMethod]
genAccessor unaryMethods, (CategoryDecl, OpaqueDecl, NewtypeDecl, ClassDecl, ProtocolDecl, TaggedKindDecl, ValueKindDecl), ptr Table[MethodID, UnaryMethod]

genAccessor instSingleMethodVTable, (ClassDecl, ProtocolDecl, TaggedKindDecl, ValueKindDecl), ptr Table[TypeID, Table[MethodID, SingleMethod]]
genAccessor instMultiMethodVTable, (ClassDecl, ProtocolDecl, TaggedKindDecl, ValueKindDecl), ptr Table[TypeID, Table[MethodID, MultiMethod]]
genAccessor instCastMethodVTable, (ClassDecl, ProtocolDecl, TaggedKindDecl, ValueKindDecl), ptr Table[TypeID, Table[MethodID, CastMethod]]
genAccessor binaryMethodVTable, (ClassDecl, ProtocolDecl, TaggedKindDecl, ValueKindDecl), ptr Table[TypeID, Table[MethodID, BinaryMethod]]
genAccessor unaryMethodVTable, (ClassDecl, ProtocolDecl, TaggedKindDecl, ValueKindDecl), ptr Table[TypeID, Table[MethodID, UnaryMethod]]

genAccessor parents, (ModuleDecl, ClassDecl, ProtocolDecl, TaggedKindDecl, ValueKindDecl), ptr seq[TypeRef]

genAccessor staticInit, (CategoryDecl, NewtypeDecl, ModuleDecl, ClassDecl, ProtocolDecl, TaggedKindDecl), nil Opcodes
genAccessor defaultInit, (ClassDecl, ProtocolDecl, TaggedKindDecl), nil Opcodes