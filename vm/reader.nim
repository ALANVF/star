import util
import dec64
import ids
import typeref
import typevar
import opcode
import decls
import methods
import world
import std/tables
import std/streams
import std/typetraits

{.experimental: "notNil".}
{.push warning[User]: off.}
{.experimental: "codeReordering".}
{.pop.}

public:
 type
    Input = FileStream


proc readIntType(input: Input, t: typedesc): t {.inline.} =
    when t is int8: input.readInt8
    elif t is uint8: input.readUint8
    elif t is int16: input.readInt16
    elif t is uint16: input.readUint16
    elif t is int32: input.readInt32
    elif t is uint32: input.readUint32
    elif t is int64: input.readInt64
    elif t is uint64: input.readUint64

template genReadIntType(t: typedesc): untyped =
    proc `read t`(input: Input): t {.inline.} = input.readIntType(t) # bytecode uses 1-based ids

proc initTable[K, V](table: var Table[K, V], initialSize: int) {.inline.} =
    table = initTable[K, V](initialSize)

genReadIntType TypeID
genReadIntType TypeVarID
genReadIntType MethodID
genReadIntType InitID
genReadIntType MemberID
genReadIntType KindTag

genReadIntType LocalID
genReadIntType FieldID
genReadIntType LoopID

proc readVStr(input: Input, str: var string) =
    let size = input.readUint32
    str.setLen(size)
    input.readStr(size.int, str)

proc readVStr(input: Input): string =
    let size = input.readUint32
    return input.readStr(size.int)


proc loadWorld*(input: Input): World =
    result = World()
    
    if input.readStr(6) != "STARVM":
        raiseAssert "Not a StarVM bytecode file"
    
    result.version = (input.readUint8, input.readUint8, input.readUint8)

    result.entrypoint = (
        input.readTypeID,
        input.readMethodID
    )

    result.defaultValue = input.readTypeId
    result.defaultMultiKind = input.readTypeId
    result.defaultVoid = input.readTypeId
    result.defaultBool = input.readTypeId
    result.defaultInt8 = input.readTypeId
    result.defaultInt16 = input.readTypeId
    result.defaultInt32 = input.readTypeId
    result.defaultInt64 = input.readTypeId
    result.defaultUInt8 = input.readTypeId
    result.defaultUInt16 = input.readTypeId
    result.defaultUInt32 = input.readTypeId
    result.defaultUInt64 = input.readTypeId
    result.defaultFloat32 = input.readTypeId
    result.defaultFloat64 = input.readTypeId
    result.defaultDec64 = input.readTypeId
    result.defaultChar = input.readTypeId
    result.defaultStr = input.readTypeId
    result.defaultPtr = input.readTypeId
    result.defaultIterable1 = input.readTypeId
    result.defaultIterable2 = input.readTypeId
    result.defaultIterator1 = input.readTypeId
    result.defaultIterator2 = input.readTypeId
    result.defaultFunc0 = input.readTypeId
    result.defaultFunc1 = input.readTypeId
    result.defaultFunc2 = input.readTypeId
    result.defaultFunc3 = input.readTypeId

    result.defaultVoidRef = TypeRef(kind: trDecl, declID: result.defaultVoid)
    result.defaultBoolRef = TypeRef(kind: trDecl, declID: result.defaultBool)
    result.defaultInt8Ref = TypeRef(kind: trDecl, declID: result.defaultInt8)
    result.defaultInt16Ref = TypeRef(kind: trDecl, declID: result.defaultInt16)
    result.defaultInt32Ref = TypeRef(kind: trDecl, declID: result.defaultInt32)
    result.defaultInt64Ref = TypeRef(kind: trDecl, declID: result.defaultInt64)
    result.defaultUInt8Ref = TypeRef(kind: trDecl, declID: result.defaultUInt8)
    result.defaultUInt16Ref = TypeRef(kind: trDecl, declID: result.defaultUInt16)
    result.defaultUInt32Ref = TypeRef(kind: trDecl, declID: result.defaultUInt32)
    result.defaultUInt64Ref = TypeRef(kind: trDecl, declID: result.defaultUInt64)
    result.defaultFloat32Ref = TypeRef(kind: trDecl, declID: result.defaultFloat32)
    result.defaultFloat64Ref = TypeRef(kind: trDecl, declID: result.defaultFloat64)
    result.defaultDec64Ref = TypeRef(kind: trDecl, declID: result.defaultDec64)
    result.defaultCharRef = TypeRef(kind: trDecl, declID: result.defaultChar)
    result.defaultStrRef = TypeRef(kind: trDecl, declID: result.defaultStr)

    let numDecls = input.readUint32
    result.typeDecls.setLen(numDecls)
    shallow result.typeDecls
    for i in 0..<numDecls:
        result.typeDecls[i] = input.readDecl


proc readDecl(input: Input): BaseDecl =
    case input.readUint8
    of 0: input.readCategoryDecl
    of 1: input.readOpaqueDecl
    of 2: input.readNewtypeDecl
    of 3: input.readModuleDecl
    of 4: input.readClassDecl
    of 5: input.readProtocolDecl
    of 6: input.readTaggedKindDecl
    of 7: input.readValueKindDecl
    else: raiseAssert "bad"

proc readDeclPrelude[T: BaseDecl](input: Input, decl: T) =
    decl.id = input.readTypeID
    input.readVStr decl.name
    input.readTypeVars decl.typevars

    input.readSingleMethods decl.staticSingleMethods
    input.readMultiMethods decl.staticMultiMethods

proc readTypeVars(input: Input, tvars: var TypeVars) =
    let size = input.readUint8
    initTable tvars, size.int
    for _ in times(size):
        let tvar = input.readTypeVarDecl
        tvars[tvar.id] = tvar

proc readTypeVarDecl(input: Input): TypeVarDecl =
    result = TypeVarDecl()
    result.id = input.readTypeVarID
    input.readVStr result.name
    input.readParents result.parents

proc readParents(input: Input, parents: var seq[TypeRef]) =
    let size = input.readUint8
    parents.setLen(size)
    for i in 0'u8..<size:
        input.readTypeRef parents[i]

proc readCategoryDecl(input: Input): CategoryDecl =
    result = CategoryDecl()
    input.readDeclPrelude result

    input.readTypeRef result.pathType
    input.readTypeRef result.forType

    input.readMembers result.staticMembers

    input.readOptOpcodes result.staticInit

    input.readSingleInits result.singleInits
    input.readMultiInits result.multiInits

    input.readSingleMethods result.instSingleMethods
    input.readMultiMethods result.instMultiMethods
    input.readCastMethods result.instCastMethods
    input.readBinaryMethods result.binaryMethods
    input.readUnaryMethods result.unaryMethods

    input.readOptOpcodes result.staticDeinit

proc readOpaqueDecl(input: Input): OpaqueDecl =
    result = OpaqueDecl()
    input.readDeclPrelude result

    input.readSingleMethods result.instSingleMethods
    input.readMultiMethods result.instMultiMethods
    input.readCastMethods result.instCastMethods
    input.readBinaryMethods result.binaryMethods
    input.readUnaryMethods result.unaryMethods

proc readNewtypeDecl(input: Input): NewtypeDecl =
    result = NewtypeDecl()
    input.readDeclPrelude result

    input.readTypeRef result.base
    result.noInherit = input.readBool

    input.readMembers result.staticMembers

    input.readOptOpcodes result.staticInit

    input.readSingleMethods result.instSingleMethods
    input.readMultiMethods result.instMultiMethods
    input.readCastMethods result.instCastMethods
    input.readBinaryMethods result.binaryMethods
    input.readUnaryMethods result.unaryMethods

    input.readOptOpcodes result.staticDeinit

proc readModuleDecl(input: Input): ModuleDecl =
    result = ModuleDecl()
    input.readDeclPrelude result

    input.readParents result.parents
    
    input.readMembers result.staticMembers

    input.readOptOpcodes result.staticInit
    
    input.readOptOpcodes result.staticDeinit

proc readClassDecl(input: Input): ClassDecl =
    result = ClassDecl()
    input.readDeclPrelude result

    input.readParents result.parents

    input.readMembers result.staticMembers
    input.readMembers result.instMembers

    input.readOptOpcodes result.staticInit
    input.readOptOpcodes result.defaultInit

    input.readSingleInits result.singleInits
    input.readMultiInits result.multiInits
    
    input.readSingleMethods result.instSingleMethods
    input.readMultiMethods result.instMultiMethods
    input.readCastMethods result.instCastMethods
    input.readBinaryMethods result.binaryMethods
    input.readUnaryMethods result.unaryMethods

    input.readVTableMethods result.instSingleMethodVTable, readSingleMethods
    input.readVTableMethods result.instMultiMethodVTable, readMultiMethods
    input.readVTableMethods result.instCastMethodVTable, readCastMethods
    input.readVTableMethods result.binaryMethodVTable, readBinaryMethods
    input.readVTableMethods result.unaryMethodVTable, readUnaryMethods

    input.readOptOpcodes result.deinit
    input.readOptOpcodes result.staticDeinit

proc readProtocolDecl(input: Input): ProtocolDecl =
    result = ProtocolDecl()
    input.readDeclPrelude result

    input.readParents result.parents

    input.readMembers result.staticMembers
    input.readMembers result.instMembers

    input.readOptOpcodes result.staticInit
    input.readOptOpcodes result.defaultInit

    input.readSingleInits result.singleInits
    input.readMultiInits result.multiInits
    
    input.readSingleMethods result.instSingleMethods
    input.readMultiMethods result.instMultiMethods
    input.readCastMethods result.instCastMethods
    input.readBinaryMethods result.binaryMethods
    input.readUnaryMethods result.unaryMethods
    
    input.readVTableMethods result.instSingleMethodVTable, readSingleMethods
    input.readVTableMethods result.instMultiMethodVTable, readMultiMethods
    input.readVTableMethods result.instCastMethodVTable, readCastMethods
    input.readVTableMethods result.binaryMethodVTable, readBinaryMethods
    input.readVTableMethods result.unaryMethodVTable, readUnaryMethods

    input.readOptOpcodes result.deinit
    input.readOptOpcodes result.staticDeinit

proc readTaggedKindDecl(input: Input): TaggedKindDecl =
    result = TaggedKindDecl()
    input.readDeclPrelude result

    input.readParents result.parents
    result.isFlags = input.readBool

    input.readMembers result.staticMembers
    input.readMembers result.instMembers

    input.readTaggedCases result.cases

    input.readOptOpcodes result.staticInit
    input.readOptOpcodes result.defaultInit
    
    input.readSingleMethods result.instSingleMethods
    input.readMultiMethods result.instMultiMethods
    input.readCastMethods result.instCastMethods
    input.readBinaryMethods result.binaryMethods
    input.readUnaryMethods result.unaryMethods

    input.readVTableMethods result.instSingleMethodVTable, readSingleMethods
    input.readVTableMethods result.instMultiMethodVTable, readMultiMethods
    input.readVTableMethods result.instCastMethodVTable, readCastMethods
    input.readVTableMethods result.binaryMethodVTable, readBinaryMethods
    input.readVTableMethods result.unaryMethodVTable, readUnaryMethods

    input.readOptOpcodes result.deinit
    input.readOptOpcodes result.staticDeinit

proc readValueKindDecl(input: Input): ValueKindDecl =
    result = ValueKindDecl()
    input.readDeclPrelude result

    input.readParents result.parents
    result.isFlags = input.readBool
    if input.readBool:
        new result.base
        input.readTypeRef result.base[]
    else:
        result.base = nil
    
    input.readMembers result.staticMembers

    input.readValueCases result.cases

    input.readOptOpcodes result.staticInit
    
    input.readSingleMethods result.instSingleMethods
    input.readMultiMethods result.instMultiMethods
    input.readCastMethods result.instCastMethods
    input.readBinaryMethods result.binaryMethods
    input.readUnaryMethods result.unaryMethods

    input.readVTableMethods result.instSingleMethodVTable, readSingleMethods
    input.readVTableMethods result.instMultiMethodVTable, readMultiMethods
    input.readVTableMethods result.instCastMethodVTable, readCastMethods
    input.readVTableMethods result.binaryMethodVTable, readBinaryMethods
    input.readVTableMethods result.unaryMethodVTable, readUnaryMethods

    input.readOptOpcodes result.deinit
    input.readOptOpcodes result.staticDeinit


proc readInitPrelude[T: Init](input: Input, init: T) =
    init.id = input.readInitID
    input.readOpcodes init.body

proc readSingleInit(input: Input): SingleInit =
    result = SingleInit(body: cast[Opcodes](nil))
    input.readInitPrelude result

    input.readVStr result.name

proc readMultiInit(input: Input): MultiInit =
    result = MultiInit(body: cast[Opcodes](nil))
    input.readInitPrelude result

    input.readTypeVars result.typevars
    let arity = input.readUint8
    result.params.setLen(arity)
    shallow result.params
    for i in 0'u8..<arity:
        input.readTypeRef result.params[i]
    input.readVStr result.name


proc readMethodPrelude[T: Method](input: Input, mth: T) =
    mth.id = input.readMethodID
    input.readOpcodes mth.body

proc readSingleMethod(input: Input): SingleMethod =
    result = SingleMethod(body: cast[Opcodes](nil))
    input.readMethodPrelude result
    input.readVStr result.name

proc readMultiMethod(input: Input): MultiMethod =
    result = MultiMethod(body: cast[Opcodes](nil))
    input.readMethodPrelude result

    input.readTypeVars result.typevars
    let arity = input.readUint8
    result.params.setLen(arity)
    shallow result.params
    for i in 0'u8..<arity:
        input.readTypeRef result.params[i]
    input.readVStr result.name

proc readCastMethod(input: Input): CastMethod =
    result = CastMethod(body: cast[Opcodes](nil))
    input.readMethodPrelude result

    input.readTypeVars result.typevars
    input.readTypeRef result.`type`

proc readBinaryMethod(input: Input): BinaryMethod =
    result = BinaryMethod(body: cast[Opcodes](nil))
    input.readMethodPrelude result

    input.readTypeVars result.typevars
    input.readTypeRef result.param
    input.readVStr result.name

proc readUnaryMethod(input: Input): UnaryMethod =
    result = UnaryMethod(body: cast[Opcodes](nil))
    input.readMethodPrelude result

    input.readVStr result.name


proc readSingleInits(input: Input, inits: var Table[InitID, SingleInit]) =
    let size = input.readUint16
    if size > 0:
        initTable inits, size.int
        for _ in times(size):
            let init = input.readSingleInit
            inits[init.id] = init

proc readMultiInits(input: Input, inits: var Table[InitID, MultiInit]) =
    let size = input.readUint16
    if size > 0:
        initTable inits, size.int
        for _ in times(size):
            let init = input.readMultiInit
            inits[init.id] = init

proc readSingleMethods(input: Input, methods: var Table[MethodID, SingleMethod]) =
    let size = input.readUint16
    if size > 0:
        initTable methods, size.int
        for _ in times(size):
            let mth = input.readSingleMethod
            methods[mth.id] = mth

proc readMultiMethods(input: Input, methods: var Table[MethodID, MultiMethod]) =
    let size = input.readUint16
    if size > 0:
        initTable methods, size.int
        for _ in times(size):
            let mth = input.readMultiMethod
            methods[mth.id] = mth

proc readCastMethods(input: Input, methods: var Table[MethodID, CastMethod]) =
    let size = input.readUint16
    if size > 0:
        initTable methods, size.int
        for _ in times(size):
            let mth = input.readCastMethod
            methods[mth.id] = mth

proc readBinaryMethods(input: Input, methods: var Table[MethodID, BinaryMethod]) =
    let size = input.readUint16
    if size > 0:
        initTable methods, size.int
        for _ in times(size):
            let mth = input.readBinaryMethod
            methods[mth.id] = mth

proc readUnaryMethods(input: Input, methods: var Table[MethodID, UnaryMethod]) =
    let size = input.readUint16
    if size > 0:
        initTable methods, size.int
        for _ in times(size):
            let mth = input.readUnaryMethod
            methods[mth.id] = mth


template readVTableMethods[T: Method](input: Input, vtable: var Table[TypeID, Table[MethodID, T]], callback: untyped) =
    let size = input.readUint8
    if size > 0:
        initTable vtable, size.int
        for _ in times(size):
            let id = input.readTypeID
            var t: Table[MethodID, T]
            input.callback t
            vtable[id] = t



proc readMember(input: Input): Member =
    result = Member()
    result.id = input.readMemberID
    input.readTypeRef result.`type`

proc readMembers(input: Input, members: var Table[MemberID, Member]) =
    let size = input.readUint16
    initTable members, size.int
    for _ in times(size):
        let member = input.readMember
        members[member.id] = member


proc readTaggedCase(input: Input): TaggedCase =
    result = TaggedCase()
    result.id = input.readKindTag
    input.readVStr result.name
    
    let arity = input.readUint8
    if arity == 0:
        result.slots = nil
    else:
        new result.slots
        result.slots[].setLen(arity)
        for i in 0'u8..<arity:
            input.readTypeRef result.slots[][i]
    
    input.readOptOpcodes result.defaultInit

proc readValueCase(input: Input): ValueCase =
    result = ValueCase()
    result.id = input.readKindTag
    input.readVStr result.name
    input.readOptOpcodes result.valueInit


proc readTaggedCases(input: Input, cases: var Table[KindTag, TaggedCase]) =
    let size = input.readUint16
    initTable cases, size.int
    for _ in times(size):
        let tcase = input.readTaggedCase
        cases[tcase.id] = tcase

proc readValueCases(input: Input, cases: var Table[KindTag, ValueCase]) =
    let size = input.readUint16
    initTable cases, size.int
    for _ in times(size):
        let vcase = input.readValueCase
        cases[vcase.id] = vcase


proc readTypeInstCtx(input: Input, ctx: var TypeInstCtx)
proc readTypeRef(input: Input, tref: var TypeRef) =
    {.cast(uncheckedAssign).}:
        tref.kind = KTypeRef(input.readUint8)
    case tref.kind
    of trDecl:
        tref.declID = input.readTypeID
    of trInst:
        tref.instID = input.readTypeID
        input.readTypeInstCtx tref.instCtx
    of trTypeVar:
        input.readTypeVar tref.tvar
    of trThis:
        discard

proc readTypeInstCtx(input: Input, ctx: var TypeInstCtx) =
    let size = input.readUint8
    initTable ctx, size.int
    for _ in times(size):
        let id = input.readTypeVarID
        var t: TypeRef
        input.readTypeRef t
        ctx[id] = t

proc readTypeVar(input: Input, tvar: var TypeVar) =
    tvar.kind = KTypeVar(input.readUint8)
    tvar.id = input.readTypeVarID

#[proc readTypeVarInstCtx(input: Input, ctx: var TypeVarInstCtx) =
    let size = input.readUint8
    new ctx
    initTable ctx[], size.int
    for _ in times(size):
        var id: TypeVar
        input.readTypeVar id
        input.readTypeRef ctx[][id]]#

proc readOptTypeVarInstCtx(input: Input, ctx: var nil TypeVarInstCtx) =
    let size = input.readUint8
    if size == 0:
        ctx = nil
    else:
        new ctx
        initTable ctx[], size.int
        for _ in times(size):
            var id: TypeVar
            input.readTypeVar id
            
            ctx[][id] = default typeof ctx[][id]

            let entry = addr ctx[][id]
            input.readTypeRef entry[].t
            if input.readBool:
                new entry[].mappings
                let mappings = entry[].mappings

                for value in fields(mappings[]):
                    let size = input.readUint8
                    if size > 0:
                        type T = typeof(value).genericParams.get(0)
                        initTable value, size.int
                        for _ in times(size):
                            value[input.readIntType(T)] = (input.readTypeID, input.readIntType(T))

            else:
                entry[].mappings = nil


proc readOpcode(input: Input, op: var Opcode)
proc readOpcodes(input: Input, ops: var Opcodes) =
    let size = input.readUint32
    new ops
    ops[].setLen(size)
    shallow ops[]
    for i in 0..<size:
        input.readOpcode ops[][i]

proc readOptOpcodes(input: Input, ops: var nil Opcodes) =
    let size = input.readUint32
    if size == 0:
        ops = nil
    else:
        new ops
        ops[].setLen(size)
        shallow ops[]
        for i in 0..<size:
            input.readOpcode ops[][i]

proc readOpcode(input: Input, op: var Opcode) =
    {.cast(uncheckedAssign).}:
        op.kind = KOpcode(input.readUint8)
    case op.kind
    of oNewLocal, oDup..oPop, oRet, oRetVoid, oRethrow, oTrue..oThis, oKindID, oKindValue:
        discard
    of oGetLocal..oTeeLocal:
        op.localID = input.readLocalID
    of oGetField..oTeeStaticField:
        op.fieldID = input.readFieldID
    of oIf, oIfNot:
        input.readOpcodes op.ifThen
    of oIfElse:
        input.readOpcodes op.then
        input.readOpcodes op.`else`
    of oDo:
        op.do_id = input.readLoopID
        input.readOpcodes op.do_body
    of oLoop:
        op.loop_id = input.readLoopID
        input.readOpcodes op.loop_body
        input.readOptOpcodes op.loop_then
    of oTry:
        input.readOpcodes op.`try`
        input.readOpcodes op.catch
    of oThrow:
        input.readVStr op.throw
    of oBreak, oNext:
        op.labelID = input.readLoopID
    of oNative:
        op.native = NativeOp(input.readUint16)
    of oInt8: op.i8 = input.readInt8
    of oUInt8: op.u8 = input.readUint8
    of oInt16: op.i16 = input.readInt16
    of oUInt16: op.u16 = input.readUint16
    of oInt32: op.i32 = input.readInt32
    of oUInt32: op.u32 = input.readUint32
    of oInt64: op.i64 = input.readInt64
    of oUInt64: op.u64 = input.readUint64
    of oFloat32: op.f32 = input.readFloat32
    of oFloat64: op.f64 = input.readFloat64
    of oDec64: op.d64 = toDec64(input.readInt64)
    of oChar: op.c = input.readUint8
    of oStr: input.readVStr op.s
    of oBlock:
        input.readOpcodes op.`block`
    of oVCaseID, oTCaseID:
        input.readTypeRef op.cid_t
        op.cid_tag = input.readKindTag
    of oKindSlot:
        op.slot = input.readUint8
    of oUpcast..oDynamicCast:
        input.readTypeRef op.target
    of oOfType:
        input.readTypeRef op.of_t
    of oNewPtr, oPtrFromAddr:
        input.readTypeRef op.ptr_t
    of oGetMember..oTeeMember:
        op.instID = input.readMemberID
    of oGetStaticMember..oTeeStaticMember:
        input.readTypeRef op.owner
        op.staticID = input.readMemberID
    of oDefaultInit:
        input.readTypeRef op.di_t
    of oInitThis_s, oInitThis_m:
        input.readTypeRef op.init_t
        op.init_id = input.readInitID
        input.readOptTypeVarInstCtx op.init_ctx
    of oSend_is:
        input.readTypeRef op.is_t
        op.is_id = input.readInitID
    of oSend_im:
        input.readTypeRef op.im_t
        op.im_id = input.readInitID
        input.readOptTypeVarInstCtx op.im_ctx
    of oSend_ss:
        input.readTypeRef op.ss_t
        op.ss_id = input.readMethodID
    of oSend_ms:
        input.readTypeRef op.ms_t
        op.ms_id = input.readMethodID
        input.readOptTypeVarInstCtx op.ms_ctx
    of oSend_si, oSendDyn_si:
        input.readTypeRef op.si_t
        op.si_id = input.readMethodID
    of oSend_mi, oSendDyn_mi:
        input.readTypeRef op.mi_t
        op.mi_id = input.readMethodID
        input.readOptTypeVarInstCtx op.mi_ctx
    of oSend_c, oSendDyn_c:
        input.readTypeRef op.cm_t
        op.cm_id = input.readMethodID
        input.readOptTypeVarInstCtx op.cm_ctx
    of oSend_bo, oSendDyn_bo:
        input.readTypeRef op.bo_t
        op.bo_id = input.readMethodID
        input.readOptTypeVarInstCtx op.bo_ctx
    of oSend_uo, oSendDyn_uo:
        input.readTypeRef op.uo_t
        op.uo_id = input.readMethodID
    of oInitClass:
        input.readTypeRef op.class_t
    of oInitTKind..oInitMultiVKind:
        input.readTypeRef op.kind_t
        op.kind_tag = input.readKindTag
    of oMultiKindHasTag, oMultiKindGetTag:
        op.mk_tag = input.readKindTag
    of oMultiKindGetSlot:
        op.mks_tag = input.readKindTag
        op.mks_slot = input.readUint8