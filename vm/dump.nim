import util
import dec64
import ids
import typeref
import opcode
import typevar
import methods
import decls
import world

import std/strutils
import std/tables
import std/streams
import std/with

{.experimental: "notNil".}

public:
 type
    Output = distinct FileStream

proc write(output: Output, x: string) {.borrow.}
proc close*(output: Output) {.borrow.}


const
    TAB_WIDTH = 4
    TAB = " ".repeat(TAB_WIDTH)

# TODO: no global vars
var
    level: Natural = 0
    tabs: seq[string] = @[""]

proc indent(output: Output) =
    if tabs.len - 1 < level:
        tabs.add tabs[^1] & TAB
    
    output.write tabs[level]

proc newline(output: Output) =
    output.write "\n"
    output.indent

proc writeLine(output: Output, x: string) =
    output.write x
    output.newline

template writeBlock(output: Output, blk: untyped) =
    `output`.write "{"
    inc level
    `blk`
    dec level
    `output`.newline
    `output`.write "}"


proc write(output: Output, typevars: TypeVars)
proc write(output: Output, decl: TypeVarDecl)
proc writeParents(output: Output, parents: seq[TypeRef])


proc write(output: Output, decl: BaseDecl)
proc write(output: Output, decl: CategoryDecl)
proc write(output: Output, decl: OpaqueDecl)
proc write(output: Output, decl: NewtypeDecl)
proc write(output: Output, decl: ModuleDecl)
proc write(output: Output, decl: ClassDecl)
proc write(output: Output, decl: ProtocolDecl)
proc write(output: Output, decl: TaggedKindDecl)
proc write(output: Output, decl: ValueKindDecl)

proc write(output: Output, mths: Table[MethodID, SingleMethod], isStatic: bool)
proc write(output: Output, mths: Table[MethodID, MultiMethod], isStatic: bool)
proc write(output: Output, mths: Table[MethodID, CastMethod])
proc write(output: Output, mths: Table[MethodID, BinaryMethod])
proc write(output: Output, mths: Table[MethodID, UnaryMethod])
proc write(output: Output, inits: Table[InitID, SingleInit])
proc write(output: Output, inits: Table[InitID, MultiInit])
proc write(output: Output, mems: Table[MemberID, Member], isStatic: bool)
proc write(output: Output, tcases: Table[KindTag, TaggedCase])
proc write(output: Output, vcases: Table[KindTag, ValueCase])
proc write[T: Method](output: Output, vtable: Table[TypeID, Table[MethodID, T]])

proc write(output: Output, mth: SingleMethod, isStatic: bool)
proc write(output: Output, mth: MultiMethod, isStatic: bool)
proc write(output: Output, mth: CastMethod)
proc write(output: Output, mth: BinaryMethod)
proc write(output: Output, mth: UnaryMethod)
proc write(output: Output, init: SingleInit)
proc write(output: Output, init: MultiInit)
proc write(output: Output, mem: Member, isStatic: bool)
proc write(output: Output, tcase: TaggedCase)
proc write(output: Output, vcase: ValueCase)

proc write(output: Output, prefix: string, ops: nil Opcodes)

proc write(output: Output, typeref: TypeRef)
proc write(output: Output, ctx: TypeInstCtx)

proc write(output: Output, tvar: TypeVar)

proc write(output: Output, ctx: nil TypeVarInstCtx)
proc write(output: Output, mappings: ref TypeVarInstCtxEntryMappings)
proc write(output: Output, name: string, mappings: Table[MemberID, (TypeID, MemberID)], isStatic: bool)
proc write(output: Output, name: string, mappings: Table[KindTag, (TypeID, KindTag)])
proc write(output: Output, name: string, mappings: Table[InitID, (TypeID, InitID)])
proc write(output: Output, name: string, mappings: Table[MethodID, (TypeID, MethodID)], isStatic, isOp: bool)

proc write(output: Output, ops: Opcodes)
proc write(output: Output, op: Opcode)

proc write*(output: Output, world: World) =
    let (major, minor, patch) = world.version
    with output:
        writeLine "version " & $major & "." & $minor & "." & $patch

        writeLine "entrypoint decl#" & $world.entrypoint[0] & " " & $world.entrypoint[1]

        newline

        write "types "
        writeBlock: with output:
            newline
            writeLine "Value = decl#" & $world.defaultValue
            writeLine "MultiKind = decl#" & $world.defaultMultiKind
            writeLine "Int8 = decl#" & $world.defaultInt8
            writeLine "Int16 = decl#" & $world.defaultInt16
            writeLine "Int32 = decl#" & $world.defaultInt32
            writeLine "Int64 = decl#" & $world.defaultInt64
            writeLine "UInt8 = decl#" & $world.defaultUInt8
            writeLine "UInt16 = decl#" & $world.defaultUInt16
            writeLine "UInt32 = decl#" & $world.defaultUInt32
            writeLine "UInt64 = decl#" & $world.defaultUInt64
            writeLine "Float32 = decl#" & $world.defaultFloat32
            writeLine "Float64 = decl#" & $world.defaultFloat64
            writeLine "Dec64 = decl#" & $world.defaultDec64
            writeLine "Char = decl#" & $world.defaultChar
            writeLine "Str = decl#" & $world.defaultStr
            writeLine "Ptr = decl#" & $world.defaultPtr
            writeLine "VoidPtr = decl#" & $world.defaultVoidPtr
            writeLine "Iterable1 = decl#" & $world.defaultIterable1
            writeLine "Iterable2 = decl#" & $world.defaultIterable2
            writeLine "Iterator1 = decl#" & $world.defaultIterator1
            writeLine "Iterator2 = decl#" & $world.defaultIterator2
            writeLine "Func0 = decl#" & $world.defaultFunc0
            writeLine "Func1 = decl#" & $world.defaultFunc1
            writeLine "Func2 = decl#" & $world.defaultFunc2
            write "Func3 = decl#" & $world.defaultFunc3
        newline
    
    for decl in world.typeDecls:
        output.newline
        output.write decl
        output.newline


proc write(output: Output, typevars: TypeVars) =
    for _, td in typevars:
        output.write td
        output.newline

proc write(output: Output, decl: TypeVarDecl) =
    output.write "type " & $decl.id & " " & decl.name
    output.writeParents decl.parents
        

proc writeParents(output: Output, parents: seq[TypeRef]) =
    if parents.len > 0:
        output.write " of #["
        inc level

        for parent in parents:
            output.newline
            output.write parent
        
        dec level
        output.newline
        output.write "]"
    


proc write(output: Output, decl: BaseDecl) =
    if decl of CategoryDecl: output.write cast[CategoryDecl](decl)
    elif decl of OpaqueDecl: output.write cast[OpaqueDecl](decl)
    elif decl of NewtypeDecl: output.write cast[NewtypeDecl](decl)
    elif decl of ModuleDecl: output.write cast[ModuleDecl](decl)
    elif decl of ClassDecl: output.write cast[ClassDecl](decl)
    elif decl of ProtocolDecl: output.write cast[ProtocolDecl](decl)
    elif decl of TaggedKindDecl: output.write cast[TaggedKindDecl](decl)
    elif decl of ValueKindDecl: output.write cast[ValueKindDecl](decl)

proc write(output: Output, decl: CategoryDecl) =
    output.write decl.typevars
    
    with output:
        write "category " & $decl.id & " " & decl.name & " "
        write decl.pathType
        write " for "
        write decl.forType
        write " "
        writeBlock: with output:
            write decl.staticMembers, true

            write "init is static", decl.staticInit

            write decl.singleInits
            write decl.multiInits
            write decl.staticSingleMethods, true
            write decl.staticMultiMethods, true
            write decl.instSingleMethods, false
            write decl.instMultiMethods, false
            write decl.instCastMethods
            write decl.binaryMethods
            write decl.unaryMethods

            write "deinit is static", decl.staticDeinit

proc write(output: Output, decl: OpaqueDecl) =
    output.write decl.typevars
    
    output.write "opaquetype " & $decl.id & " " & decl.name & " "
    output.writeBlock:
        with output:
            write decl.staticSingleMethods, true
            write decl.staticMultiMethods, true
            write decl.instSingleMethods, false
            write decl.instMultiMethods, false
            write decl.instCastMethods
            write decl.binaryMethods
            write decl.unaryMethods

proc write(output: Output, decl: NewtypeDecl) =
    output.write decl.typevars
    
    with output:
        write "newtype " & $decl.id & " " & decl.name & " ("
        write decl.base
        write (if decl.noInherit: ") is noinherit " else: ") ")
        writeBlock: with output:
            write decl.staticMembers, true
            
            write "init is static", decl.staticInit
            
            write decl.staticSingleMethods, true
            write decl.staticMultiMethods, true
            write decl.instSingleMethods, false
            write decl.instMultiMethods, false
            write decl.instCastMethods
            write decl.binaryMethods
            write decl.unaryMethods

            write "deinit is static", decl.staticDeinit

proc write(output: Output, decl: ModuleDecl) =
    output.write decl.typevars
    
    output.write "module " & $decl.id & " " & decl.name
    output.writeParents decl.parents
    output.write " "
    output.writeBlock: with output:
        write decl.staticMembers, true
        
        write "init is static", decl.staticInit
        
        write decl.staticSingleMethods, true
        write decl.staticMultiMethods, true

        write "deinit is static", decl.staticDeinit

proc write(output: Output, decl: ClassDecl) =
    output.write decl.typevars
    
    output.write "class " & $decl.id & " " & decl.name
    output.writeParents decl.parents
    output.write " "
    output.writeBlock: with output:
        write decl.staticMembers, true
        write decl.instMembers, false
        
        write "init is static", decl.staticInit
        write "init", decl.defaultInit
        
        write decl.singleInits
        write decl.multiInits
        write decl.staticSingleMethods, true
        write decl.staticMultiMethods, true
        write decl.instSingleMethods, false
        write decl.instMultiMethods, false
        write decl.instCastMethods
        write decl.binaryMethods
        write decl.unaryMethods

        write decl.instSingleMethodVTable
        write decl.instMultiMethodVTable
        write decl.instCastMethodVTable
        write decl.binaryMethodVTable
        write decl.unaryMethodVTable

        write "deinit", decl.deinit
        write "deinit is static", decl.staticDeinit

proc write(output: Output, decl: ProtocolDecl) =
    output.write decl.typevars
    
    output.write "protocol " & $decl.id & " " & decl.name
    output.writeParents decl.parents
    output.write " "
    output.writeBlock: with output:
        write decl.staticMembers, true
        write decl.instMembers, false
        
        write "init is static", decl.staticInit
        write "init", decl.defaultInit
        
        write decl.singleInits
        write decl.multiInits
        write decl.staticSingleMethods, true
        write decl.staticMultiMethods, true
        write decl.instSingleMethods, false
        write decl.instMultiMethods, false
        write decl.instCastMethods
        write decl.binaryMethods
        write decl.unaryMethods

        write decl.instSingleMethodVTable
        write decl.instMultiMethodVTable
        write decl.instCastMethodVTable
        write decl.binaryMethodVTable
        write decl.unaryMethodVTable

        write "deinit", decl.deinit
        write "deinit is static", decl.staticDeinit

proc write(output: Output, decl: TaggedKindDecl) =
    output.write decl.typevars
    
    output.write "tagged-kind " & $decl.id & " " & decl.name
    output.writeParents decl.parents
    if decl.isFlags:
        output.write " is flags "
    else:
        output.write " "
    output.writeBlock: with output:
        write decl.staticMembers, true
        write decl.instMembers, false

        write decl.cases
        
        write "init is static", decl.staticInit
        write "init", decl.defaultInit
        
        write decl.staticSingleMethods, true
        write decl.staticMultiMethods, true
        write decl.instSingleMethods, false
        write decl.instMultiMethods, false
        write decl.instCastMethods
        write decl.binaryMethods
        write decl.unaryMethods

        write decl.instSingleMethodVTable
        write decl.instMultiMethodVTable
        write decl.instCastMethodVTable
        write decl.binaryMethodVTable
        write decl.unaryMethodVTable

        write "deinit", decl.deinit
        write "deinit is static", decl.staticDeinit

proc write(output: Output, decl: ValueKindDecl) =
    output.write decl.typevars
    
    output.write "value-kind " & $decl.id & " " & decl.name
    if decl.base != nil:
        with output:
            write " ("
            write decl.base[]
            write ")"
    output.writeParents decl.parents
    if decl.isFlags:
        output.write " is flags "
    else:
        output.write " "
    output.writeBlock: with output:
        write decl.staticMembers, true
        
        write decl.cases

        write "init is static", decl.staticInit
        
        write decl.staticSingleMethods, true
        write decl.staticMultiMethods, true
        write decl.instSingleMethods, false
        write decl.instMultiMethods, false
        write decl.instCastMethods
        write decl.binaryMethods
        write decl.unaryMethods

        write decl.instSingleMethodVTable
        write decl.instMultiMethodVTable
        write decl.instCastMethodVTable
        write decl.binaryMethodVTable
        write decl.unaryMethodVTable

        write "deinit", decl.deinit
        write "deinit is static", decl.staticDeinit


proc write(output: Output, mths: Table[MethodID, SingleMethod], isStatic: bool) =
    if mths.len > 0:
        for _, mth in mths:
            output.newline
            output.write mth, isStatic
            output.newline

proc write(output: Output, mths: Table[MethodID, MultiMethod], isStatic: bool) =
    if mths.len > 0:
        for _, mth in mths:
            output.newline
            output.write mth, isStatic
            output.newline

proc write(output: Output, mths: Table[MethodID, CastMethod]) =
    if mths.len > 0:
        for _, mth in mths:
            output.newline
            output.write mth
            output.newline

proc write(output: Output, mths: Table[MethodID, BinaryMethod]) =
    if mths.len > 0:
        for _, mth in mths:
            output.newline
            output.write mth
            output.newline

proc write(output: Output, mths: Table[MethodID, UnaryMethod]) =
    if mths.len > 0:
        for _, mth in mths:
            output.newline
            output.write mth
            output.newline

proc write(output: Output, inits: Table[InitID, SingleInit]) =
    if inits.len > 0:
        for _, init in inits:
            output.newline
            output.write init
            output.newline

proc write(output: Output, inits: Table[InitID, MultiInit]) =
    if inits.len > 0:
        for _, init in inits:
            output.newline
            output.write init
            output.newline

proc write(output: Output, mems: Table[MemberID, Member], isStatic: bool) =
    if mems.len > 0:
        for _, mem in mems:
            output.newline
            output.write mem, isStatic
            output.newline
    
proc write(output: Output, tcases: Table[KindTag, TaggedCase]) =
    if tcases.len > 0:
        for _, tcase in tcases:
            output.newline
            output.write tcase
            output.newline
    
proc write(output: Output, vcases: Table[KindTag, ValueCase]) =
    if vcases.len > 0:
        for _, vcase in vcases:
            output.newline
            output.write vcase
            output.newline

proc write[T: Method](output: Output, vtable: Table[TypeID, Table[MethodID, T]]) =
    for tid, mths in vtable:
        output.newline
        output.write "from decl#" & $tid & " "
        output.writeBlock:
            when compiles(output.write(mths, false)):
                output.write mths, false
            elif compiles(output.write(mths)):
                output.write mths
            
        output.newline


proc write(output: Output, mth: SingleMethod, isStatic: bool) =
    output.write "on " & $mth.id & " [" & mth.name & "] "
    if isStatic: output.write "is static "
    output.write mth.body

proc write(output: Output, mth: MultiMethod, isStatic: bool) =
    output.write mth.typevars

    output.write "on " & $mth.id & " ["
    inc level

    var i = 0
    let params = mth.params
    for label in mth.name.split(" "):
        with output:
            newline
            write label & " " & $i & " ("
            write params[i]
            write ")"
        inc i

    dec level
    output.newline
    output.write "] "
    if isStatic: output.write "is static "
    output.write mth.body

proc write(output: Output, mth: CastMethod) =
    output.write mth.typevars
    
    with output:
        write "on " & $mth.id & " ["
        write mth.`type`
        write "] "
        write mth.body

proc write(output: Output, mth: BinaryMethod) =
    output.write mth.typevars
    
    output.write "on " & $mth.id & " `" & mth.name & "` ["
    inc level
    output.newline

    output.write "0 ("
    output.write mth.param
    output.write ")"

    dec level
    output.newline
    output.write "] "
    output.write mth.body


proc write(output: Output, mth: UnaryMethod) =
    output.write "on " & $mth.id & " `" & mth.name & "` "
    output.write mth.body

proc write(output: Output, init: SingleInit) =
    output.write "on " & $init.id & " [" & init.name & "] "
    output.write init.body

proc write(output: Output, init: MultiInit) =
    output.write init.typevars
    
    output.write "on " & $init.id & " ["
    inc level

    var i = 0
    let params = init.params
    for label in init.name.split(" "):
        with output:
            newline
            write label & " " & $i & " ("
            write params[i]
            write ")"
        inc i

    dec level
    output.newline
    output.write "] "
    output.write init.body

proc write(output: Output, mem: Member, isStatic: bool) =
    with output:
        write "my " & $mem.id & " ("
        write mem.`type`
        write (if isStatic: ") is static" else: ")")

proc write(output: Output, tcase: TaggedCase) =
    output.write "has " & $tcase.id & " ["
    
    if tcase.slots == nil:
        output.write tcase.name & "]"
    else:
        inc level

        var i = 0
        let slots = tcase.slots[]
        for label in tcase.name.split(" "):
            with output:
                newline
                write label & " " & $i & " ("
                write slots[i]
                write ")"
            inc i

        dec level
        output.newline
        output.write "]"
    
    if tcase.defaultInit != nil:
        output.write " "
        output.write tcase.defaultInit

proc write(output: Output, vcase: ValueCase) =
    output.write "has " & $vcase.id & " " & vcase.name
    if vcase.valueInit != nil:
        output.write " "
        output.write vcase.valueInit


proc write(output: Output, prefix: string, ops: nil Opcodes) =
    if ops != nil:
        with output:
            newline
            write prefix & " "
            write ops
            newline


proc write(output: Output, typeref: TypeRef) =
    case typeref.kind
    of trDecl:
        output.write "decl#" & $typeref.declID
    of trInst:
        output.write "inst#" & $typeref.instID
        output.write typeref.instCtx
    of trTypeVar:
        output.write typeref.tvar
    of trThis:
        output.write "this"

proc write(output: Output, ctx: TypeInstCtx) =
    output.writeBlock:
        for tv, t in ctx:
            output.newline
            output.write "dvar#" & $tv & " => "
            output.write t


proc write(output: Output, tvar: TypeVar) =
    case tvar.kind
        of tvDecl: output.write "dvar#" & $tvar.id
        of tvMethod: output.write "mvar#" & $tvar.id


proc write(output: Output, ctx: nil TypeVarInstCtx) =
    if ctx != nil:
        output.write " "
        output.writeBlock:
            for tv, (t, mappings) in ctx:
                with output:
                    newline
                    write tv
                    write " => "
                    write t
                    write mappings

proc write(output: Output, mappings: ref TypeVarInstCtxEntryMappings) =
    if mappings != nil:
        let maps = mappings[]
        output.write " "
        output.writeBlock: with output:
            write "static-members", maps.staticMembers, isStatic=true
            write "inst-members", maps.instMembers, isStatic=false
            write "tagged-cases", maps.taggedCases
            write "value-cases", maps.valueCases
            write "single-inits", maps.singleInits
            write "multi-inits", maps.multiInits
            write "static-single-methods", maps.staticSingleMethods, isStatic=true, isOp=false
            write "static-multi-methods", maps.staticMultiMethods, isStatic=true, isOp=false
            write "inst-single-methods", maps.instSingleMethods, isStatic=false, isOp=false
            write "inst-multi-methods", maps.instMultiMethods, isStatic=false, isOp=false
            write "cast-methods", maps.instCastMethods, isStatic=false, isOp=false
            write "binary-operators", maps.binaryMethods, isStatic=false, isOp=true
            write "unary-operators", maps.unaryMethods, isStatic=false, isOp=true

proc write(output: Output, name: string, mappings: Table[MemberID, (TypeID, MemberID)], isStatic: bool) =
    if mappings.len > 0:
        output.write name & " "
        let isStatic = if isStatic: " is static" else: ""
        output.writeBlock:
            for tv_id, (tid, from_id) in mappings:
                output.newline
                output.write "my " & $tv_id & isStatic & " => decl#" & $tid & " " & $from_id

proc write(output: Output, name: string, mappings: Table[KindTag, (TypeID, KindTag)]) =
    if mappings.len > 0:
        output.write name & " "
        output.writeBlock:
            for tv_id, (tid, from_id) in mappings:
                output.newline
                output.write "has " & $tv_id & " => decl#" & $tid & " " & $from_id

proc write(output: Output, name: string, mappings: Table[InitID, (TypeID, InitID)]) =
    if mappings.len > 0:
        output.write name & " "
        output.writeBlock:
            for tv_id, (tid, from_id) in mappings:
                output.newline
                output.write "init " & $tv_id & " => decl#" & $tid & " " & $from_id

proc write(output: Output, name: string, mappings: Table[MethodID, (TypeID, MethodID)], isStatic, isOp: bool) =
    if mappings.len > 0:
        output.write name & " "
        let prefix = if isOp: "operator " else: "on "
        let isStatic = if isStatic: " is static" else: ""
        output.writeBlock:
            for tv_id, (tid, from_id) in mappings:
                output.newline
                output.write prefix & $tv_id & isStatic & " => decl#" & $tid & " " & $from_id



proc write(output: Output, ops: Opcodes) =
    output.writeBlock:
        for op in ops[]:
            output.newline
            output.write op

proc write(output: Output, op: Opcode) =
    case op.kind
        # Storage / Access
        of oNewLocal: output.write "new-local"
        of oGetLocal: output.write "get-local " & $op.localID
        of oSetLocal: output.write "set-local " & $op.localID
        of oTeeLocal: output.write "tee-local " & $op.localID
        of oGetField: output.write "get-field " & $op.fieldID
        of oSetField: output.write "set-field " & $op.fieldID
        of oTeeField: output.write "tee-field " & $op.fieldID
        of oGetStaticField: output.write "get-static-field " & $op.fieldID
        of oSetStaticField: output.write "set-static-field " & $op.fieldID
        of oTeeStaticField: output.write "tee-static-field " & $op.fieldID

        # Stack manip
        of oDup: output.write "dup"
        of oDup2: output.write "dup2"
        of oSwap: output.write "swap"
        of oPop: output.write "pop"

        # Control flow
        of oIf:
            output.write "if "
            output.write op.ifThen
        of oIfNot:
            output.write "if-not "
            output.write op.ifThen
        of oIfElse:
            output.write "if "
            output.write op.then
            output.write " else "
            output.write op.`else`
        of oDo:
            output.write "do " & $op.do_id & " "
            output.write op.do_body
        of oLoop:
            output.write "loop " & $op.loop_id & " "
            output.write op.loop_body
            if op.loop_then != nil:
                output.write " then "
                output.write op.loop_then
        of oTry:
            output.write "try "
            output.write op.`try`
            output.write " catch "
            output.write op.`catch`
        of oRet: output.write "ret"
        of oRetVoid: output.write "retvoid"
        of oThrow: output.write "throw " & op.throw.escape
        of oRethrow: output.write "rethrow"
        of oBreak: output.write "break " & $op.label_id
        of oNext: output.write "next " & $op.label_id

        # Natives
        of oNative: output.write "native " & $op.native

        # Values
        of oInt8: output.write "int8 " & $op.i8
        of oUInt8: output.write "uint8 " & $op.u8
        of oInt16: output.write "int16 " & $op.i16
        of oUInt16: output.write "uint16 " & $op.u16
        of oInt32: output.write "int32 " & $op.i32
        of oUInt32: output.write "uint32 " & $op.u32
        of oInt64: output.write "int64 " & $op.i64
        of oUInt64: output.write "uint64 " & $op.u64
        of oFloat32: output.write "float32 " & $op.f32
        of oFloat64: output.write "float64 " & $op.f64
        of oDec64: output.write "dec64 " & $op.d64
        of oChar: output.write "char " & ($op.c.char).escape("#\"", "\"")
        of oStr: output.write "str " & op.s.escape
        of oTrue: output.write "true"
        of oFalse: output.write "false"
        of oThis: output.write "this"
        #oFunc,

        # Comprehension
        of oBlock:
            output.write "block "
            output.write op.`block`

        # Operations
        of oVCaseID:
            output.write "vcase-id "
            output.write op.cid_t
            output.write " " & $op.cid_tag
        of oTCaseID:
            output.write "tcase-id "
            output.write op.cid_t
            output.write " " & $op.cid_tag
        of oKindID: output.write "kind-id"
        of oKindSlot: output.write "kind-slot " & $op.slot
        of oKindValue: output.write "kind-value"
        of oUpcast: output.write "upcast "; output.write op.target
        of oDowncast: output.write "downcast "; output.write op.target
        of oNativeCast: output.write "nativecast "; output.write op.target
        of oDynamicCast: output.write "dynamiccast "; output.write op.target
        of oOfType: output.write "of-type "; output.write op.of_t
        of oNewPtr: output.write "new-ptr "; output.write op.ptr_t
        of oPtrFromAddr: output.write "ptr-from-addr "; output.write op.ptr_t

        # Members
        of oGetMember: output.write "get-member " & $op.instID
        of oSetMember: output.write "set-member " & $op.instID
        of oTeeMember: output.write "tee-member " & $op.instID
        of oGetStaticMember:
            output.write "get-static-member "
            output.write op.owner
            output.write " " & $op.staticID
        of oSetStaticMember:
            output.write "set-static-member "
            output.write op.owner
            output.write " " & $op.staticID
        of oTeeStaticMember:
            output.write "tee-static-member "
            output.write op.owner
            output.write " " & $op.staticID

        # Messaging
        of oDefaultInit:
            output.write "default-init"
            output.write op.di_t
        of oInitThis_s:
            output.write "init-this-s "
            output.write op.init_t
            output.write " " & $op.init_id
        of oInitThis_m:
            output.write "init-this-m "
            output.write op.init_t
            output.write " " & $op.init_id
            output.write op.init_ctx
        of oSend_is:
            output.write "send-init-s "
            output.write op.is_t
            output.write " " & $op.is_id
        of oSend_im:
            output.write "send-init-m "
            output.write op.im_t
            output.write " " & $op.im_id
            output.write op.im_ctx
        of oSend_ss:
            output.write "send-ss "
            output.write op.ss_t
            output.write " " & $op.ss_id
        of oSend_ms:
            output.write "send-ms "
            output.write op.ms_t
            output.write " " & $op.ms_id
            output.write op.ms_ctx
        of oSend_si:
            output.write "send-si "
            output.write op.si_t
            output.write " " & $op.si_id
        of oSendDyn_si:
            output.write "send-dyn-si "
            output.write op.si_t
            output.write " " & $op.si_id
        of oSend_mi:
            output.write "send-mi "
            output.write op.mi_t
            output.write " " & $op.mi_id
            output.write op.mi_ctx
        of oSendDyn_mi:
            output.write "send-dyn-mi "
            output.write op.mi_t
            output.write " " & $op.mi_id
            output.write op.mi_ctx
        of oSend_c:
            output.write "send-c "
            output.write op.cm_t
            output.write " " & $op.cm_id
            output.write op.cm_ctx
        of oSendDyn_c:
            output.write "send-dyn-c "
            output.write op.cm_t
            output.write " " & $op.cm_id
            output.write op.cm_ctx
        of oSend_bo:
            output.write "send-bo "
            output.write op.bo_t
            output.write " " & $op.bo_id
            output.write op.bo_ctx
        of oSendDyn_bo:
            output.write "send-dyn-bo "
            output.write op.bo_t
            output.write " " & $op.bo_id
            output.write op.bo_ctx
        of oSend_uo:
            output.write "send-uo "
            output.write op.uo_t
            output.write " " & $op.uo_id
        of oSendDyn_uo:
            output.write "send-dyn-uo "
            output.write op.uo_t
            output.write " " & $op.uo_id

        # Creation
        of oInitClass:
            output.write "init-class "
            output.write op.class_t
        of oInitTKind:
            output.write "init-tkind "
            output.write op.kind_t
            output.write " " & $op.kind_tag
        of oInitVKind:
            output.write "init-vkind "
            output.write op.kind_t
            output.write " " & $op.kind_tag
        of oInitMultiTKind:
            output.write "init-multi-tkind "
            output.write op.kind_t
            output.write " " & $op.kind_tag
        of oInitMultiVKind:
            output.write "init-multi-vkind "
            output.write op.kind_t
            output.write " " & $op.kind_tag

        # Multi kinds
        of oMultiKindHasTag: output.write "multi-kind-has-tag " & $op.mk_tag
        of oMultiKindGetTag: output.write "multi-kind-get-tag " & $op.mk_tag
        of oMultiKindGetSlot: output.write "multi-kind-get-slot" & $op.mks_tag & " " & $op.mks_slot