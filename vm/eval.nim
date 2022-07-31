import util
import dec64
import ids
import typeref
import opcode
import values
#import typevar
import methods
import decls
import world

import math
import macros
import std/strutils
import std/tables

{.experimental: "notNil".}

public:
 type
    State = ref object
        world: World
        thisDecl: BaseDecl
        thisType: TypeRef
        thisValue: ptr Value
        thisTVCtx: ptr TypeInstCtx
        methodTVCtx: nil TypeVarInstCtx
        #methodKind: MethodKind
        staticFields, instFields: ref seq[Value]

        stack: seq[Value]
    
    Scope = ref object
        locals: seq[Value]
    
    KResult = enum
        rReturn,
        rReturnVoid,
        rThrow,
        rRethrow,
        rBreak,
        rNext
    Result = ref object
        case kind: KResult
        of rReturnVoid: nil
        of rReturn:
            value: Value
        of rThrow:
            infos: seq[string]
            thrown: Value
        of rRethrow: nil
        of rBreak, rNext:
            id: LoopID

func makeInt8(state: State, value: int8, t = state.world.defaultInt8Ref): Value = Value(t: t, kind: vInt8, i8: value)
func makeUInt8(state: State, value: uint8, t = state.world.defaultUInt8Ref): Value = Value(t: t, kind: vUInt8, u8: value)
func makeInt16(state: State, value: int16, t = state.world.defaultInt16Ref): Value = Value(t: t, kind: vInt16, i16: value)
func makeUInt16(state: State, value: uint16, t = state.world.defaultUInt16Ref): Value = Value(t: t, kind: vUInt16, u16: value)
func makeInt32(state: State, value: int32, t = state.world.defaultInt32Ref): Value = Value(t: t, kind: vInt32, i32: value)
func makeUInt32(state: State, value: uint32, t = state.world.defaultUInt32Ref): Value = Value(t: t, kind: vUInt32, u32: value)
func makeInt64(state: State, value: int64, t = state.world.defaultInt64Ref): Value = Value(t: t, kind: vInt64, i64: value)
func makeUInt64(state: State, value: uint64, t = state.world.defaultUInt64Ref): Value = Value(t: t, kind: vUInt64, u64: value)
func makeFloat32(state: State, value: float32, t = state.world.defaultFloat32Ref): Value = Value(t: t, kind: vFloat32, f32: value)
func makeFloat64(state: State, value: float64, t = state.world.defaultFloat64Ref): Value = Value(t: t, kind: vFloat64, f64: value)
func makeDec64(state: State, value: Dec64, t = state.world.defaultDec64Ref): Value = Value(t: t, kind: vDec64, d64: value)
func makeBool(state: State, value: bool, t = state.world.defaultBoolRef): Value = Value(t: t, kind: vBool, b: value)
func makeChar(state: State, value: uint8, t = state.world.defaultCharRef): Value = Value(t: t, kind: vUInt8, u8: value.uint8)
func makeKindID(state: State, value: KindTag): Value =
    when KindTag is int8: makeInt8(state, value)
    elif KindTag is uint8: makeUInt8(state, value)
    elif KindTag is int16: makeInt16(state, value)
    elif KindTag is uint16: makeUInt16(state, value)
    elif KindTag is int32: makeInt32(state, value)
    elif KindTag is uint32: makeUInt32(state, value)
    elif KindTag is int64: makeInt64(state, value)
    elif KindTag is uint64: makeUInt64(state, value)
    else: assert(false, "???")
func makeStr(state: State, value: string): Value =
    let pchar = state.world.makePtrTo(state.world.defaultCharRef)
    var buf: seq[Value]
    newSeq(buf, value.len)
    for i, c in value.pairs:
        buf[i] = state.makeChar(c.uint8)
    
    let rbuf = new(seq[Value])
    rbuf[] = buf

    let buffer = Value(t: pchar, kind: vPtr, `ptr`: rbuf)
    let length = state.makeInt32(value.len.int32)
    let capacity = state.makeInt32(value.len.int32)
    var fields: ref seq[Value] not nil
    new(fields)
    fields[] = @[buffer, length, capacity]
    
    return Value(t: state.world.defaultStrRef, kind: vClass, c_fields: fields)


proc lookupTypevar(state: State, tv: TypeVar): TypeRef =
    if tv.kind == tvDecl and state.thisTVCtx != nil and state.thisTVCtx[].contains(tv.id):
        return state.thisTVCtx[][tv.id]
    
    if state.methodTVCtx != nil and state.methodTVCtx.contains(tv):
        return state.methodTVCtx[tv].t

    assert(false, "???")

# TODO: add a way to differentiate between concrete instances and non-concrete instances (aka they have typevars/this)
proc getInCtx(state: State, tref: TypeRef): TypeRef =
    case tref.kind
    of trDecl:
        tref
    
    of trInst:
        var ctx: TypeInstCtx = tref.instCtx
        for value in ctx.mvalues:
            value = state.getInCtx(value)
        TypeRef(kind: trInst, instID: tref.instID, instCtx: ctx)
    
    of trTypeVar:
        state.lookupTypevar(tref.tvar)
    
    of trThis:
        state.thisType

proc getDecl(state: State, tref: TypeRef): BaseDecl =
    case tref.kind
    of trDecl: state.world.typeDecls[tref.declID]
    of trInst: state.world.typeDecls[tref.instID]
    of trThis: state.thisDecl
    of trTypeVar: raiseAssert "???"

proc getDeclName(state: State, tref: TypeRef): string =
    case tref.kind
    of trDecl: state.world.typeDecls[tref.declID].name
    of trInst: state.world.typeDecls[tref.instID].name&"[...]"
    of trThis: "this("&state.thisDecl.name&")"
    of trTypeVar: raiseAssert "???" #state.getDeclName(state.lookupTypevar(tref.tvar))


proc staticFieldsFor(state: State, t: TypeRef): ref seq[Value] =
    case t.kind:
    of trThis:
        return state.staticFields
    else:
        return state.world.staticFields.getOrDefault(state.getDecl(t).id, nil)

proc defaultInitFor(state: State, t: TypeRef): nil Opcodes =
    let decl =
        case t.kind:
        of trDecl: state.world.typeDecls[t.declID]
        of trInst: state.world.typeDecls[t.instID]
        of trThis: state.thisDecl
        of trTypeVar: raiseAssert "???"
    
    return decl.defaultInit


proc ofType(state: State, d1, d2: BaseDecl, ctx1, ctx2: ptr TypeInstCtx): bool

proc ofType(state: State, t1, t2: TypeRef): bool =
    case t1.kind
    of trDecl:
        case t2.kind
        of trDecl:
            t1.declID == t2.declID or
                state.ofType(state.world.typeDecls[t1.declID], state.world.typeDecls[t2.declID], nil, nil)
        of trInst:
            state.ofType(state.world.typeDecls[t1.declID], state.world.typeDecls[t2.instID], nil, addr t2.instCtx)
        of trThis:
            state.ofType(t1, state.thisType)
        of trTypeVar:
            state.ofType(t1, state.lookupTypevar(t2.tvar))
    of trInst:
        case t2.kind
        of trDecl:
            state.ofType(state.world.typeDecls[t1.instID], state.world.typeDecls[t1.declID], addr t1.instCtx, nil)
        of trInst:
            state.ofType(state.world.typeDecls[t1.instID], state.world.typeDecls[t2.instID], addr t1.instCtx, addr t2.instCtx)
        of trThis:
            state.ofType(t1, state.thisType)
        of trTypeVar:
            state.ofType(t1, state.lookupTypevar(t2.tvar))
    of trThis:
        state.ofType(state.thisType, t2)
    of trTypeVar:
        state.ofType(state.lookupTypevar(t1.tvar), t2)

proc ofType(state: State, d1, d2: BaseDecl, ctx1, ctx2: ptr TypeInstCtx): bool =
    if d1 == d2: return true

    # TODO
    false


proc stringy(state: State, value: Value): string =
    let dname = state.getDeclName(value.t)
    let tname = dname & " "

    case value.kind
    of vVoid: tname & "()"
    of vBool: tname & $value.b
    of vInt8: tname & $value.i8
    of vUInt8:
        if value.t == state.world.defaultCharRef:
            tname & value.u8.char.`$`.escape("'", "'")
        else:
            tname & $value.u8
    of vInt16: tname & $value.i16
    of vUInt16: tname & $value.u16
    of vInt32: tname & $value.i32
    of vUInt32: tname & $value.u32
    of vInt64: tname & $value.i64
    of vUInt64: tname & $value.u64
    of vFloat32: tname & $value.f32
    of vFloat64: tname & $value.f64
    of vDec64: tname & $value.d64
    of vPtr:
        if value.offset == 0:
            tname & "0x" & cast[uint64](value.`ptr`).toHex
        else:
            tname & (cast[uint64](value.`ptr`) + cast[uint64](value.offset)).toHex
    of vVoidPtr:
        tname & "0x" & cast[uint64](value.vptr).toHex
    
    of vClass:
        if value.t == state.world.defaultStrRef:
            let length = value.c_fields[2].i32
            let data = value.c_fields[0].`ptr`[]
            var res: string
            res.setLen(length)
            shallow res
            
            for i, val in data:
                res[i] = val.u8.char
            
            tname & res.escape()
        else:
            var res: seq[string] = @[]
            for v in value.c_fields[]: res.add state.stringy(v)
            dname & "[new: " & res.join(", ") & "]"
    else:
        raiseAssert "NYI!"


#[proc initStaticFields*(world: World, typeID: TypeID): ref seq[Value] =
    if world.staticFields.hasKey(typeID):
        return world.staticFields[typeID]

    let decl = world.typeDecls[typeID]
    if decl isnot OpaqueDecl:
        let fields = decl.staticMembers
        if fields != nil:
            let numFields = fields[].len
            if numFields > 0:
                new result
                newSeq(result[], numFields)
                


            else:
                result = nil
        else:
            result = nil
    else:
        result = nil]#


proc newState*(world: World, decl: BaseDecl): State =
    let declID = decl.id
    let t = TypeRef(kind: trDecl, declID: declID)
    let staticFields = world.getOrInitStaticFields(decl)

    return State(
        world: world,
        thisDecl: decl,
        thisType: t,
        thisValue: nil,
        thisTVCtx: nil,
        methodTVCtx: nil,
        staticFields: staticFields,
        instFields: nil,
        stack: @[]
    )

proc newState(state: State, t: ptr TypeRef, value: ptr Value, methodTCtx: nil TypeVarInstCtx = nil): State =
    let world = state.world
    let ty = t[]
    let (decl, tctx) =
        case ty.kind
        of trDecl: (world.typeDecls[ty.declID], nil)
        of trInst: (world.typeDecls[ty.instID], addr t[].instCtx)
        of trThis: (state.thisDecl, state.thisTVCtx)
        of trTypeVar: raiseAssert "???"
    
    return State(
        world: world,
        thisDecl: decl,
        thisType: ty,
        thisValue: value,
        thisTVCtx: tctx,
        methodTVCtx: methodTCtx,
        staticFields: state.staticFieldsFor(t[]),
        instFields: block:
            if value == nil:
                nil
            else:
                let val = value[]
                case val.kind
                of vClass: cast[ref seq[Value]](val.c_fields) # T not nil should be implicitly compatible with nil T :(
                of vTKindClass: val.tkc_fields
                else: nil,
        stack: @[]
    )


proc newClass(state: State, t: TypeRef): Value =
    let decl =
        case t.kind
        of trDecl: state.world.typeDecls[t.declID]
        of trInst: state.world.typeDecls[t.instID]
        else: raise newException(ValueError, "bad")
    let clsDecl =
        if decl of ClassDecl:
            cast[ClassDecl](decl)
        else:
            raise newException(ValueError, "bad")
    
    let numFields = clsDecl.instMembers.len
    var fields: ref seq[Value] not nil
    new(fields)
    newSeq(fields[], numFields)

    return Value(t: t, kind: vClass, c_fields: fields)


proc eval*(state: State, scope: Scope, ops: Opcodes): Result

proc searchCastMethods(state: State, t, target: TypeRef): nil CastMethod =
    let decl = state.getDecl(t)
    let ops = decl.instCastMethods

    if ops == nil:
        let parents = decl.parents

        if parents == nil:
            return nil

        # TODO: change this to be a breadth search
        for parent in parents[]:
            let res = state.searchCastMethods(parent, target)
            if res != nil:
                return res
        
        return nil
    else:
        for op in ops[].values:
            # TODO: implement proper behavior for generics and inheritance
            if target == op.`type`:
                return op
        
        return nil

proc dynCast(state: State, value: Value, target: TypeRef): Either[Value, Result] =
    let t = value.t
    
    if t == target:
        return Either[Value, Result].makeLeft value
    elif state.ofType(t, target):
        # TODO
        return Either[Value, Result].makeLeft value
    elif state.ofType(target, t):
        # TODO
        return Either[Value, Result].makeLeft value
    else:
        let castMethod = state.searchCastMethods(t, target)

        if castMethod == nil:
            raiseAssert "Invalid dynamic cast from `" & $t & "` to `" & $target & "`"
        
        let mstate = state.newState(addr t, addr value, nil)
        let mscope = Scope(locals: @[])

        let res = eval(mstate, mscope, castMethod.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                raiseAssert "???"
            of rReturn:
                return Either[Value, Result].makeLeft res.value
            else:
                return Either[Value, Result].makeRight res
        else:
            raiseAssert "???"


proc eval*(state: State, scope: Scope, op: Opcode): Result

proc eval*(state: State, scope: Scope, ops: Opcodes): Result =
    for op in ops[]:
        let res = eval(state, scope, op)
        if unlikely(res != nil): return res

proc evalBlock*(state: State, scope: Scope, ops: Opcodes): Result =
    let numLocals = scope.locals.len
    let res = eval(state, scope, ops)
    if scope.locals.len != numLocals:
        scope.locals.setLen(numLocals)
    return res


proc eval*(state: State, scope: Scope, native: NativeOp): Result

proc eval*(state: State, scope: Scope, op: Opcode): Result =
    case op.kind
    # Storage / Access
    of oNewLocal:
        scope.locals.setLen(scope.locals.len + 1)
    
    of oGetLocal:
        state.stack.add(scope.locals[op.localID])
    
    of oSetLocal:
        scope.locals[op.localID] = state.stack.pop
    
    of oTeeLocal:
        scope.locals[op.localID] = state.stack[^1]
    

    of oGetField:
        if unlikely(state.instFields == nil): raise newException(AccessViolationDefect, "Attempt to access instance field in static method")
        state.stack.add(state.instFields[op.fieldID])
    
    of oSetField:
        if unlikely(state.instFields == nil): raise newException(AccessViolationDefect, "Attempt to access instance field in static method")
        state.instFields[op.fieldID] = state.stack.pop
    
    of oTeeField:
        if unlikely(state.instFields == nil): raise newException(AccessViolationDefect, "Attempt to access instance field in static method")
        state.instFields[op.fieldID] = state.stack[^1]
    

    of oGetStaticField:
        if unlikely(state.staticFields == nil): raise newException(AccessViolationDefect, "Attempt to access static field in non-concrete type")
        state.stack.add(state.staticFields[op.fieldID])
    
    of oSetStaticField:
        if unlikely(state.staticFields == nil): raise newException(AccessViolationDefect, "Attempt to access static field in non-concrete type")
        state.staticFields[op.fieldID] = state.stack.pop
    
    of oTeeStaticField:
        if unlikely(state.staticFields == nil): raise newException(AccessViolationDefect, "Attempt to access static field in non-concrete type")
        state.staticFields[op.fieldID] = state.stack[^1]
    

    # Stack manip
    of oDup:
        state.stack.add(state.stack[^1])
    
    of oDup2:
        state.stack.add(state.stack[^2])
    
    of oSwap:
        swap(state.stack[^1], state.stack[^2])
    
    of oPop:
        discard state.stack.pop
    

    # Control flow
    of oIf:
        let cond = state.stack.pop
        if likely(cond.kind == vBool):
            if cond.b:
                return evalBlock(state, scope, op.ifThen)
        else:
            raise newException(ValueError, "Expected a bool value")
    
    of oIfNot:
        let cond = state.stack.pop
        if likely(cond.kind == vBool):
            if not cond.b:
                return evalBlock(state, scope, op.ifThen)
        else:
            raise newException(ValueError, "Expected a bool value")
    
    of oIfElse:
        let cond = state.stack.pop
        if likely(cond.kind == vBool):
            if cond.b:
                return evalBlock(state, scope, op.then)
            else:
                return evalBlock(state, scope, op.`else`)
        else:
            raise newException(ValueError, "Expected a bool value")
    
    of oDo:
        let id = op.do_id
        let body = op.do_body
        
        while true:
            let res = evalBlock(state, scope, body)
            if res != nil:
                case res.kind:
                of rBreak:
                    if res.id == id:
                        break
                    else:
                        return res
                of rNext:
                    if res.id != id:
                        return res
                else:
                    return res
            else:
                break
    
    of oLoop:
        let id = op.loop_id
        let body = op.loop_body
        let then = op.loop_then

        if then == nil:
            while true:
                let res = evalBlock(state, scope, body)
                if res != nil:
                    case res.kind:
                    of rBreak:
                        if res.id == id:
                            break
                        else:
                            return res
                    of rNext:
                        if res.id != id:
                            return res
                    else:
                        return res
        else:
            while true:
                let res = evalBlock(state, scope, body)
                if res != nil:
                    case res.kind:
                    of rBreak:
                        if res.id == id:
                            break
                        else:
                            return res
                    of rNext:
                        if res.id != id:
                            return res
                    else:
                        return res
                
                let res2 = evalBlock(state, scope, then)
                if res2 != nil:
                    case res2.kind:
                    of rBreak:
                        if res2.id == id:
                            break
                        else:
                            return res2
                    of rNext:
                        if res2.id != id:
                            return res2
                    else:
                        return res2
    
    of oTry:
        let body = op.`try`
        let catch = op.catch

        let res = evalBlock(state, scope, body)
        if res != nil:
            if res.kind == rThrow:
                state.stack.add(res.thrown)
                let res2 = evalBlock(state, scope, catch)
                if res2 != nil:
                    case res2.kind
                    of rThrow:
                        res2.infos.add(res.infos)
                        return res2
                    of rRethrow:
                        return res
                    else:
                        return res2
            else:
                return res
    
    of oRet:
        return Result(kind: rReturn, value: state.stack.pop)

    of oRetVoid:
        return Result(kind: rReturnVoid)

    of oThrow:
        return Result(kind: rThrow, infos: @[op.throw], thrown: state.stack.pop)

    of oRethrow:
        return Result(kind: rRethrow)
    
    of oBreak:
        return Result(kind: rBreak, id: op.labelID)

    of oNext:
        return Result(kind: rNext, id: op.labelID)

    
    # Natives
    of oNative:
        return eval(state, scope, op.native)
    

    # Values
    of oInt8: state.stack.add state.makeInt8(op.i8)
    of oUInt8: state.stack.add state.makeUInt8(op.u8)
    of oInt16: state.stack.add state.makeInt16(op.i16)
    of oUInt16: state.stack.add state.makeUInt16(op.u16)
    of oInt32: state.stack.add state.makeInt32(op.i32)
    of oUInt32: state.stack.add state.makeUInt32(op.u32)
    of oInt64: state.stack.add state.makeInt64(op.i64)
    of oUInt64: state.stack.add state.makeUInt64(op.u64)
    
    of oFloat32: state.stack.add state.makeFloat32(op.f32)
    of oFloat64: state.stack.add state.makeFloat64(op.f64)
    of oDec64: state.stack.add state.makeDec64(op.d64)

    of oChar: state.stack.add state.makeChar(op.c)
    
    of oStr: state.stack.add state.makeStr(op.s)

    of oTrue: state.stack.add state.makeBool(true)
    of oFalse: state.stack.add state.makeBool(false)

    of oThis:
        if unlikely(state.thisValue == nil): raise newException(AccessViolationDefect, "Attempt to access `this` value in static method")
        state.stack.add(state.thisValue[])
    

    # Comprehension
    of oBlock:
        let res = evalBlock(state, scope, op.`block`)
        if res != nil:
            case res.kind
            of rReturn:
                state.stack.add(res.value)
            of rReturnVoid:
                discard
            else:
                return res
        else:
            raiseAssert "Missing return value"
    

    # Operations
    of oVCaseID, oTCaseID:
        state.stack.add state.makeKindID(op.cid_tag)
    
    of oKindID:
        let value = state.stack.pop
        let id =
            case value.kind
            of vVKind: value.vk_id
            of vTKind: value.tk_id
            of vTKindClass: value.tkc_id
            else: raise newException(AccessViolationDefect, "Attempt to access the tag of a non-kind value")
        
        state.stack.add state.makeKindID(id)
    
    of oKindSlot:
        let slot = op.slot
        let value = state.stack.pop
        case value.kind
        of vTKind:
            if unlikely(value.tk_slots == nil):
                raise newException(AccessViolationDefect, "Attempt to access slots of 0-arity tagged kind value")
            else:
                state.stack.add value.tk_slots[][slot]
        of vTKindClass:
            if unlikely(value.tkc_slots == nil):
                raise newException(AccessViolationDefect, "Attempt to access slots of 0-arity tagged kind value")
            else:
                state.stack.add value.tkc_slots[][slot]
        else:
            assert false, "Expected tagged kind"
    
    of oKindValue:
        let value = state.stack.pop
        case value.kind
        of vVKind:
            if value.vk_value == nil:
                raise newException(AccessViolationDefect, "Attempt to access the non-existent value of a value kind")
            else:
                state.stack.add value.vk_value[]
        else:
            raise newException(AccessViolationDefect, "Attempt to access the value of a non-value kind value")
    
    # TODO
    of oUpcast..oNativeCast:
        var value = state.stack.pop
        value.t = state.getInCtx(op.target)
        state.stack.add value
    
    of oDynamicCast:
        var value = state.stack.pop
        let target = state.getInCtx(op.target)
        
        let res = state.dynCast(value, target)
        case res.kind
        of eLeft: state.stack.add res.left
        of eRight: return res.right


    of oOfType:
        state.stack.add state.makeBool(state.ofType(state.stack.pop.t, op.of_t))
    

    of oNewPtr:
        let sizeValue = state.stack.pop; assert likely(sizeValue.kind in {vInt8..vUInt64}), "Expected an integer"

        let size =
            case sizeValue.kind
            of vInt8: sizeValue.i8.uint
            of vUInt8: sizeValue.u8.uint
            of vInt16: sizeValue.i16.uint
            of vUInt16: sizeValue.u16.uint
            of vInt32: sizeValue.i32.uint
            of vUInt32: sizeValue.u32.uint
            of vInt64: sizeValue.i64.uint
            of vUInt64: sizeValue.u64.uint
            else: 0 # impossible
        
        let elemType = state.getInCtx(op.ptr_t)
        
        var buf: ref seq[Value]
        new(buf)
        newSeq(buf[], size)

        state.stack.add Value(t: state.world.makePtrTo(elemType), kind: vPtr, offset: 0, `ptr`: buf)
    
    of oPtrFromAddr:
        # NOTE: does not keep track of offset
        let addrValue = state.stack.pop; assert likely(addrValue.kind == vUInt64), "Expected a uint64"

        let elemType = state.getInCtx(op.ptr_t)
        
        state.stack.add Value(t: state.world.makePtrTo(elemType), kind: vPtr, offset: 0, `ptr`: cast[ref seq[Value]](addrValue.u64))
    
    
    # Members
    of oGetMember:
        let obj = state.stack.pop
        case obj.kind
        of vClass:
            state.stack.add obj.c_fields[op.instID]
        of vTKindClass:
            state.stack.add obj.tkc_fields[op.instID]
        else:
            raise newException(AccessViolationDefect, "Cannot access field because value does not have any fields")
    
    of oSetMember:
        let value = state.stack.pop
        let obj = state.stack.pop
        case obj.kind
        of vClass:
            obj.c_fields[op.instID] = value
        of vTKindClass:
            obj.tkc_fields[op.instID] = value
        else:
            raise newException(AccessViolationDefect, "Cannot access field because value does not have any fields")
    
    of oTeeMember:
        let value = state.stack.pop
        let obj = state.stack.pop
        case obj.kind
        of vClass:
            obj.c_fields[op.instID] = value
            state.stack.add value
        of vTKindClass:
            obj.tkc_fields[op.instID] = value
            state.stack.add value
        else:
            raise newException(AccessViolationDefect, "Cannot access field because value does not have any fields")
    

    of oGetStaticMember:
        let t = op.owner
        let fields = state.staticFieldsFor(t)
        if fields == nil:
            raise newException(AccessViolationDefect, "Cannot access field because type does not have any fields")
        else:
            state.stack.add fields[op.staticID]
    
    of oSetStaticMember:
        let value = state.stack.pop
        let t = op.owner
        let fields = state.staticFieldsFor(t)
        if fields == nil:
            raise newException(AccessViolationDefect, "Cannot access field because type does not have any fields")
        else:
            fields[op.staticID] = value
    
    of oTeeStaticMember:
        let value = state.stack[^1]
        let t = op.owner
        let fields = state.staticFieldsFor(t)
        if fields == nil:
            raise newException(AccessViolationDefect, "Cannot access field because type does not have any fields")
        else:
            fields[op.staticID] = value
    

    # Messaging
    of oDefaultInit:
        let t = state.getInCtx(op.di_t)
        let sender = state.stack.pop

        let di = state.defaultInitFor(t)
        if di != nil:
            let mstate = state.newState(addr t, addr sender)
            let mscope = Scope(locals: @[])
            let res = eval(mstate, mscope, di)
            if unlikely(res != nil and res.kind != rReturnVoid):
                return res
    
    of oInitThis_s:
        let t = state.getInCtx(op.init_t)

        let mstate = state.newState(addr t, state.thisValue)
        let mth = mstate.thisDecl.singleInits[][op.init_id]
        
        let mscope = Scope(locals: @[])

        let res = eval(mstate, mscope, mth.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                discard
            of rReturn:
                assert false, "???"
            else:
                return res
    
    of oInitThis_m:
        let t = state.getInCtx(op.init_t)
        
        let mstate = state.newState(addr t, state.thisValue, op.init_ctx)
        let mth = mstate.thisDecl.multiInits[][op.init_id]
        
        let numParams = mth.params.len
        var locals: seq[Value]
        setLen(locals, numParams)
        shallow(locals)
        # TODO: maybe use toOpenArray
        for i in countdown(numParams-1, 0):
            locals[i] = state.stack.pop
        let mscope = Scope(locals: locals)

        let res = eval(mstate, mscope, mth.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                discard
            of rReturn:
                assert false, "???"
            else:
                return res

    of oSend_is:
        let t = state.getInCtx(op.is_t)

        let newValue = state.newClass(t)
        let mstate = state.newState(addr t, addr newValue)
        let mth = mstate.thisDecl.singleInits[][op.is_id]
        
        let mscope = Scope(locals: @[])

        let res = eval(mstate, mscope, mth.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                discard
            of rReturn:
                assert false, "???"
            else:
                return res
        
        state.stack.add newValue

    of oSend_im:
        let t = state.getInCtx(op.im_t)

        let newValue = state.newClass(t)
        let mstate = state.newState(addr t, addr newValue, op.im_ctx)
        let mth = mstate.thisDecl.multiInits[][op.im_id]
        
        let numParams = mth.params.len
        var locals: seq[Value]
        setLen(locals, numParams)
        shallow(locals)
        # TODO: maybe use toOpenArray
        for i in countdown(numParams-1, 0):
            locals[i] = state.stack.pop
        let mscope = Scope(locals: locals)

        let res = eval(mstate, mscope, mth.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                discard
            of rReturn:
                assert false, "???"
            else:
                return res
        
        state.stack.add newValue
    
    of oSend_ss:
        let t = state.getInCtx(op.ss_t)

        let mstate = state.newState(addr t, nil)
        let mth = mstate.thisDecl.staticSingleMethods[op.ss_id]
        
        let mscope = Scope(locals: @[])

        let res = eval(mstate, mscope, mth.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                discard
            of rReturn:
                state.stack.add res.value
            else:
                return res
    
    of oSend_ms:
        let t = state.getInCtx(op.ms_t)

        let mstate = state.newState(addr t, nil, op.ms_ctx)
        let mth = mstate.thisDecl.staticMultiMethods[op.ms_id]

        let numParams = mth.params.len
        var locals: seq[Value]
        setLen(locals, numParams)
        shallow(locals)
        # TODO: maybe use toOpenArray
        for i in countdown(numParams-1, 0):
            locals[i] = state.stack.pop
        let mscope = Scope(locals: locals)

        let res = eval(mstate, mscope, mth.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                discard
            of rReturn:
                state.stack.add res.value
            else:
                return res
    
    of oSend_si:
        let t = state.getInCtx(op.si_t)

        let sender = state.stack.pop
        let mstate = state.newState(addr t, addr sender)
        let mth = mstate.thisDecl.instSingleMethods[][op.si_id]
        
        let mscope = Scope(locals: @[])

        let res = eval(mstate, mscope, mth.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                discard
            of rReturn:
                state.stack.add res.value
            else:
                return res
    
    of oSendDyn_si:
        let t = state.getInCtx(op.si_t)
        
        let sender = state.stack.pop
        let sender_t = sender.t
        
        var mstate: State
        var mth: SingleMethod
        if unlikely(t == sender_t):
            mstate = state.newState(addr t, addr sender)
            mth = mstate.thisDecl.instSingleMethods[][op.si_id]
        else:
            mstate = state.newState(addr sender_t, addr sender)
            let baseDecl = state.getDecl(t)
            mth = mstate.thisDecl.instSingleMethodVTable[][baseDecl.id][op.si_id]
        
        let mscope = Scope(locals: @[])

        let res = eval(mstate, mscope, mth.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                discard
            of rReturn:
                state.stack.add res.value
            else:
                return res

    
    of oSend_mi:
        let t = state.getInCtx(op.mi_t)

        let decl = state.getDecl(t)
        let mth = decl.instMultiMethods[][op.mi_id]

        let numParams = mth.params.len
        var locals: seq[Value]
        setLen(locals, numParams)
        shallow(locals)
        # TODO: maybe use toOpenArray
        for i in countdown(numParams-1, 0):
            locals[i] = state.stack.pop
        let mscope = Scope(locals: locals)

        let sender = state.stack.pop
        let mstate = state.newState(addr t, addr sender, op.mi_ctx)
        
        let res = eval(mstate, mscope, mth.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                discard
            of rReturn:
                state.stack.add res.value
            else:
                return res
    
    of oSendDyn_mi:
        let t = state.getInCtx(op.mi_t)

        let decl = state.getDecl(t)
        var mth = decl.instMultiMethods[][op.mi_id]

        let numParams = mth.params.len
        var locals: seq[Value]
        setLen(locals, numParams)
        shallow(locals)
        # TODO: maybe use toOpenArray
        for i in countdown(numParams-1, 0):
            locals[i] = state.stack.pop
        let mscope = Scope(locals: locals)
        
        let sender = state.stack.pop
        let sender_t = sender.t
        
        var mstate: State
        if unlikely(t == sender_t):
            mstate = state.newState(addr t, addr sender, op.mi_ctx)
        else:
            mstate = state.newState(addr sender_t, addr sender, op.mi_ctx)
            mth = decl.instMultiMethodVTable[][decl.id][op.mi_id]

        let res = eval(mstate, mscope, mth.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                discard
            of rReturn:
                state.stack.add res.value
            else:
                return res
    
    of oSend_c:
        let t = state.getInCtx(op.cm_t)

        let sender = state.stack.pop
        let mstate = state.newState(addr t, addr sender, op.cm_ctx)
        let mth = mstate.thisDecl.instCastMethods[][op.cm_id]
        
        let mscope = Scope(locals: @[])

        let res = eval(mstate, mscope, mth.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                assert false, "???"
            of rReturn:
                state.stack.add res.value
            else:
                return res
        else:
            assert false, "???"
    
    of oSendDyn_c:
        let t = state.getInCtx(op.cm_t)
        
        let sender = state.stack.pop
        let sender_t = sender.t
        
        var mstate: State
        var mth: CastMethod
        if unlikely(t == sender_t):
            mstate = state.newState(addr t, addr sender, op.cm_ctx)
            mth = mstate.thisDecl.instCastMethods[][op.cm_id]
        else:
            mstate = state.newState(addr sender_t, addr sender, op.cm_ctx)
            let baseDecl = state.getDecl(t)
            mth = mstate.thisDecl.instCastMethodVTable[][baseDecl.id][op.cm_id]
        
        let mscope = Scope(locals: @[])

        let res = eval(mstate, mscope, mth.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                assert false, "???"
            of rReturn:
                state.stack.add res.value
            else:
                return res
        else:
            assert false, "???"

    of oSend_bo:
        let t = state.getInCtx(op.bo_t)

        let arg = state.stack.pop
        let sender = state.stack.pop
        let mstate = state.newState(addr t, addr sender, op.bo_ctx)
        let mth = mstate.thisDecl.binaryMethods[][op.bo_id]
        
        let mscope = Scope(locals: @[arg])

        let res = eval(mstate, mscope, mth.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                discard
            of rReturn:
                state.stack.add res.value
            else:
                return res
    
    of oSendDyn_bo:
        let t = state.getInCtx(op.bo_t)
        
        let arg = state.stack.pop
        let sender = state.stack.pop
        let sender_t = sender.t
        
        var mstate: State
        var mth: BinaryMethod
        if unlikely(t == sender_t):
            mstate = state.newState(addr t, addr sender, op.bo_ctx)
            mth = mstate.thisDecl.binaryMethods[][op.bo_id]
        else:
            mstate = state.newState(addr sender_t, addr sender, op.bo_ctx)
            let baseDecl = state.getDecl(t)
            mth = mstate.thisDecl.binaryMethodVTable[][baseDecl.id][op.bo_id]
        
        let mscope = Scope(locals: @[arg])

        let res = eval(mstate, mscope, mth.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                discard
            of rReturn:
                state.stack.add res.value
            else:
                return res

    of oSend_uo:
        let t = state.getInCtx(op.uo_t)

        let sender = state.stack.pop
        let mstate = state.newState(addr t, addr sender)
        let mth = mstate.thisDecl.unaryMethods[][op.uo_id]
        
        let mscope = Scope(locals: @[])

        let res = eval(mstate, mscope, mth.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                discard
            of rReturn:
                state.stack.add res.value
            else:
                return res
    
    of oSendDyn_uo:
        let t = state.getInCtx(op.uo_t)
        
        let sender = state.stack.pop
        let sender_t = sender.t
        
        var mstate: State
        var mth: UnaryMethod
        if unlikely(t == sender_t):
            mstate = state.newState(addr t, addr sender)
            mth = mstate.thisDecl.unaryMethods[][op.uo_id]
        else:
            mstate = state.newState(addr sender_t, addr sender)
            let baseDecl = state.getDecl(t)
            mth = mstate.thisDecl.unaryMethodVTable[][baseDecl.id][op.uo_id]
        
        let mscope = Scope(locals: @[])

        let res = eval(mstate, mscope, mth.body)
        if res != nil:
            case res.kind
            of rReturnVoid:
                discard
            of rReturn:
                state.stack.add res.value
            else:
                return res
    

    # Creation
    of oInitClass:
        let cls = state.getInCtx(op.class_t)

        state.stack.add state.newClass(cls)
    
    of oInitTKind:
        let kind = state.getInCtx(op.kind_t)
        let decl =
            case kind.kind
            of trDecl: state.world.typeDecls[kind.declID]
            of trInst: state.world.typeDecls[kind.instID]
            else: raise newException(ValueError, "bad")
        let kindDecl =
            if decl of TaggedKindDecl:
                cast[TaggedKindDecl](decl)
            else:
                raise newException(ValueError, "bad")
        
        let tag = op.kind_tag
        let tcase = kindDecl.cases[tag]
        let numSlots = tcase.slots[].len
        let slots =
            if numSlots == 0:
                nil
            else:
                var slots2: seq[Value]
                newSeq(slots2, numSlots)
                for i in countdown(numSlots-1, 0):
                    slots2[i] = state.stack.pop

                var pslots2 = new(ref seq[Value])
                pslots2[] = slots2
                pslots2
        
        let kvalue = block:
            let numFields = kindDecl.instMembers.len
            if numFields == 0:
                Value(t: kind, kind: vTKind, tk_id: tag, tk_slots: slots)
            else:
                var fields: ref seq[Value] not nil
                new(fields)
                newSeq(fields[], numFields)
                
                Value(t: kind, kind: vTKindClass, tkc_id: tag, tkc_slots: slots, tkc_fields: fields)
        
        if tcase.defaultInit != nil:
            let mstate = state.newState(addr kind, addr kvalue)
            let mscope = Scope(locals: @[])
            let res = eval(mstate, mscope, tcase.defaultInit)
            if res != nil:
                case res.kind
                of rReturnVoid:
                    discard
                of rReturn:
                    assert false, "???"
                else:
                    return res
        
        state.stack.add kvalue
    
    of oInitVKind:
        let kind = state.getInCtx(op.kind_t)
        let decl =
            case kind.kind
            of trDecl: state.world.typeDecls[kind.declID]
            of trInst: state.world.typeDecls[kind.instID]
            else: raise newException(ValueError, "bad")
        let kindDecl =
            if decl of ValueKindDecl:
                cast[ValueKindDecl](decl)
            else:
                raise newException(ValueError, "bad")
        
        let tag = op.kind_tag
        let vcase = kindDecl.cases[tag]

        var value: ref Value
        if vcase.valueInit == nil:
            value = nil
        else:
            # TODO: should this use its own state/scope?
            let res = eval(state, scope, vcase.valueInit)
            if res != nil:
                case res.kind
                of rReturnVoid:
                    assert false, "???"
                of rReturn:
                    new(value)
                    value[] = res.value
                else:
                    return res
            else:
                assert false, "???"
        
        state.stack.add Value(t: kind, kind: vVKind, vk_id: tag, vk_value: value)
    
    of oInitMultiTKind:
        let kind = state.getInCtx(op.kind_t)
        let decl =
            case kind.kind
            of trDecl: state.world.typeDecls[kind.declID]
            of trInst: state.world.typeDecls[kind.instID]
            else: raise newException(ValueError, "bad")
        let kindDecl =
            if decl of TaggedKindDecl:
                cast[TaggedKindDecl](decl)
            else:
                raise newException(ValueError, "bad")
        
        if unlikely(not kindDecl.isFlags): raise newException(ValueError, "bad")
        
        let tag = op.kind_tag
        let tcase = kindDecl.cases[tag]
        let numSlots = tcase.slots[].len
        let slots =
            if numSlots == 0:
                nil
            else:
                var slots2: seq[Value]
                newSeq(slots2, numSlots)
                for i in countdown(numSlots-1, 0):
                    slots2[i] = state.stack.pop

                var pslots2 = new(ref seq[Value])
                pslots2[] = slots2
                pslots2
        
        let kvalue = Value(t: kind, kind: vTMultiKind, tmk_map: {tag: slots}.toTable)
        
        # should this really be allowed?
        if tcase.defaultInit != nil:
            let mstate = state.newState(addr kind, addr kvalue)
            let mscope = Scope(locals: @[])
            let res = eval(mstate, mscope, tcase.defaultInit)
            if res != nil:
                case res.kind
                of rReturnVoid:
                    discard
                of rReturn:
                    assert false, "???"
                else:
                    return res
        
        state.stack.add kvalue
    
    of oInitMultiVKind:
        let kind = state.getInCtx(op.kind_t)
        let decl =
            case kind.kind
            of trDecl: state.world.typeDecls[kind.declID]
            of trInst: state.world.typeDecls[kind.instID]
            else: raise newException(ValueError, "bad")
        let kindDecl =
            if decl of ValueKindDecl:
                cast[ValueKindDecl](decl)
            else:
                raise newException(ValueError, "bad")
        
        if unlikely(not kindDecl.isFlags): raise newException(ValueError, "bad")
        
        let tag = op.kind_tag
        let vcase = kindDecl.cases[tag]
        
        var value: ref Value
        if vcase.valueInit == nil:
            value = nil
        else:
            # TODO: should this use its own state/scope?
            let res = eval(state, scope, vcase.valueInit)
            if res != nil:
                case res.kind
                of rReturnVoid:
                    assert false, "???"
                of rReturn:
                    new(value)
                    value[] = res.value
                else:
                    return res
            else:
                assert false, "???"
        
        state.stack.add Value(t: kind, kind: vVMultiKind, vmk_map: {tag: value}.toTable)
    

    # Multi kinds
    of oMultiKindHasTag:
        let tag = op.mk_tag

        let value = state.stack.pop
        case value.kind
        of vVMultiKind:
            state.stack.add state.makeBool(value.vmk_map.contains(tag))
        of vTMultiKind:
            state.stack.add state.makeBool(value.tmk_map.contains(tag))
        else:
            assert false, "Expected multi kind"
    
    of oMultiKindGetTag:
        let tag = op.mk_tag

        let value = state.stack.pop
        case value.kind
        of vVMultiKind:
            if value.vmk_map.contains(tag):
                state.stack.add Value(t: value.t, kind: vVMultiKind, vmk_map: {tag: value.vmk_map[tag]}.toTable)
            else:
                raise newException(ValueError, "Multi kind does not contain the tag given")
        of vTMultiKind:
            if value.tmk_map.contains(tag):
                state.stack.add Value(t: value.t, kind: vTMultiKind, tmk_map: {tag: value.tmk_map[tag]}.toTable)
            else:
                raise newException(ValueError, "Multi kind does not contain the tag given")
        else:
            assert false, "Expected multi kind"
    
    of oMultiKindGetSlot:
        let tag = op.mks_tag
        let slot = op.mks_slot

        let value = state.stack.pop; assert likely(value.kind == vTMultiKind), "Expected a tagged multi kind"

        if value.tmk_map.contains(tag):
            let slots = value.tmk_map[tag]
            if unlikely(slots == nil):
                raise newException(AccessViolationDefect, "Attempt to access slots of 0-arity multi tagged kind value")
            else:
                state.stack.add slots[][slot]
        else:
            raise newException(ValueError, "Multi kind does not contain the tag given")


    #else: raiseAssert "NYI!"

    return nil

proc dynEq*(value1, value2: Value): bool =
    # TODO: take custom eq overloads into account
    if unlikely(value1.kind != value2.kind):
        false
    else:
        case value1.kind
        of vVoid: true
        of vBool: value1.b == value2.b
        of vInt8: value1.i8 == value2.i8
        of vUInt8: value1.u8 == value2.u8
        of vInt16: value1.i16 == value2.i16
        of vUInt16: value1.u16 == value2.u16
        of vInt32: value1.i32 == value2.i32
        of vUInt32: value1.u32 == value2.u32
        of vInt64: value1.i64 == value2.i64
        of vUInt64: value1.u64 == value2.u64
        of vFloat32: value1.f32 == value2.f32
        of vFloat64: value1.f64 == value2.f64
        of vDec64: value1.d64 == value2.d64
        of vPtr: cast[ptr seq[Value]](value1.`ptr`) == cast[ptr seq[Value]](value2.`ptr`)
        of vVoidPtr: value1.vptr == value2.vptr

        of vClass:
            if likely(value1.t == value2.t):
                let fields1 = value1.c_fields[]
                let fields2 = value2.c_fields[]
                for i in 0..high(fields1):
                    if not dynEq(fields1[i], fields2[i]):
                        return false
                true
            else:
                false
        
        of vVKind:
            #value1.t == value2.t and
            value1.vk_id == value2.vk_id
        
        of vTKind:
            if likely(value1.t == value2.t) and value1.tk_id == value2.tk_id:
                if value1.tk_slots == nil:
                    false
                else:
                    let slots1 = value1.tk_slots[]
                    let slots2 = value2.tk_slots[]
                    for i in 0..high(slots1):
                        if not dynEq(slots1[i], slots2[i]):
                            return false
                    true
            else:
                false
        
        of vTKindClass:
            if likely(value1.t == value2.t) and value1.tk_id == value2.tk_id:
                let fields1 = value1.tkc_fields[]
                let fields2 = value2.tkc_fields[]
                for i in 0..high(fields1):
                    if not dynEq(fields1[i], fields2[i]):
                        return false
                if value1.tkc_slots == nil:
                    false
                else:
                    let slots1 = value1.tkc_slots[]
                    let slots2 = value2.tkc_slots[]
                    for i in 0..high(slots1):
                        if not dynEq(slots1[i], slots2[i]):
                            return false
                    true
            else:
                false
        
        of vVMultiKind:
            if likely(value1.t == value2.t):
                let table1 = value1.vmk_map
                let table2 = value2.vmk_map
                if table1.len != table2.len:
                    false
                elif table1.len == 0:
                    true
                else:
                    for key in table1.keys:
                        if not table2.contains(key):
                            return false
                    true
            else:
                false
        
        of vTMultiKind:
            if likely(value1.t == value2.t):
                let table1 = value1.tmk_map
                let table2 = value2.tmk_map
                if table1.len != table2.len:
                    false
                elif table1.len == 0:
                    true
                else:
                    for key, value in table1.pairs:
                        if not table2.contains(key):
                            return false
                        elif value != nil:
                            let slots1 = value[]
                            let slots2 = table2[key][]
                            for i in 0..high(slots1):
                                if not dynEq(slots1[i], slots2[i]):
                                    return false
                    true
            else:
                false


macro unaryOp(kind, res, op: untyped): untyped =
    let kindName = ($kind)[1..^1]
    let kindDisplay = kindName.toLowerAscii
    let resName = ($res)[1..^1]
    let make = ident "make"&resName
    let value = ident "value"
    let res =
        if resName == "Str" or kindName != resName:
            quote do: state.stack.add state.`make`(`op`)
        else:
            quote do: state.stack.add state.`make`(`op`, `value`.t)
    quote do:
        let `value` = state.stack.pop
        assert `value`.kind == `kind`, "Expected " & `kindDisplay`
        `res`

template unaryOp(kind, op: untyped): untyped = unaryOp(kind, kind, op)

macro castOp(kind, res, op: untyped): untyped =
    let kindName = ($kind)[1..^1]
    let kindDisplay = kindName.toLowerAscii
    let resName = ($res)[1..^1]
    let resDisplay = resName.toLowerAscii
    let resIdent = case resDisplay
        of "dec64": ident "intToDec64"
        else: ident resDisplay
    let make = ident "make"&resName
    let value = ident "value"
    quote do:
        let `value` = state.stack.pop
        assert `value`.kind == `kind`, "Expected " & `kindDisplay`
        state.stack.add state.`make`(`op`.`resIdent`, `value`.t)

macro binaryOp(kind, res, op: untyped): untyped =
    let kindName = ($kind)[1..^1]
    let kindDisplay = kindName.toLowerAscii
    let resName = ($res)[1..^1]
    let make = ident "make"&resName
    let value1 = ident "value1"
    let value2 = ident "value2"
    let res =
        if kindName != resName:
            quote do: state.stack.add state.`make`(`op`)
        else:
            quote do: state.stack.add state.`make`(`op`, `value1`.t)
    quote do:
        let `value2` = state.stack.pop
        let `value1` = state.stack.pop
        assert `value1`.kind == `kind` and `value2`.kind == `kind`, "Expected " & `kindDisplay`
        `res`

template binaryOp(kind, op: untyped): untyped = binaryOp(kind, kind, op)

proc eval*(state: State, scope: Scope, native: NativeOp): Result =
    case native
    # all values
    of value_eq:
        let value2 = state.stack.pop
        let value1 = state.stack.pop
        
        state.stack.add state.makeBool(dynEq(value1, value2))
    
    of value_new:
        let value = state.stack.pop

        let newValue =
            case value.kind
            of vClass:
                var fields: ref seq[Value] not nil
                new(fields)
                fields[] = value.c_fields[]
                Value(t: value.t, kind: vClass, c_fields: fields)
            
            of vTKindClass:
                var fields: ref seq[Value] not nil
                new(fields)
                fields[] = value.tkc_fields[]
                Value(t: value.t, kind: vTKindClass, tkc_slots: value.tkc_slots, tkc_fields: fields)
            
            else:
                value
        
        state.stack.add newValue

    
    of cast_value_str:
        let value = state.stack.pop

        # TODO
        state.stack.add state.makeStr($value)
    
    of value_address:
        # NOTE: this will not work correctly for ref types because Value is not a ref object,
        #       therefore all addresses will never be accurate due to copying
        let value = state.stack.pop
        let address =
            case value.kind
            of vPtr: cast[uint64](value.`ptr`)
            of vVoidPtr: cast[uint64](value.vptr)
            of vClass: cast[uint64](value.c_fields)
            else: raise newException(ValueError, "bad")
            
        state.stack.add state.makeUInt64(address)
    

    # multi-kinds
    of multikind_truthy:
        let value = state.stack.pop

        let cond =
            case value.kind
            of vVMultiKind: value.vmk_map.len == 0
            of vTMultiKind: value.tmk_map.len == 0
            else: raise newException(ValueError, "Expected multi kind")
        
        state.stack.add state.makeBool(cond)
    
    of multikind_has:
        let value2 = state.stack.pop
        let value1 = state.stack.pop

        assert value1.kind == value2.kind #and value1.t == value2.t
        case value1.kind
        of vVMultiKind:
            var cond = true
            let table1 = value1.vmk_map
            let table2 = value2.vmk_map

            if table1.len < table2.len:
                cond = false
            else:
                for key in table2.keys:
                    if not table1.contains(key):
                        cond = false
                        break

            state.stack.add state.makeBool(cond)
        
        of vTMultiKind:
            var cond = true
            let table1 = value1.tmk_map
            let table2 = value2.tmk_map

            if table1.len < table2.len:
                cond = false
            else:
                block outer:
                    for key, value in table2.pairs:
                        if not table1.contains(key):
                            cond = false
                            break
                        elif value != nil:
                            let slots1 = table1[key][]
                            let slots2 = value[]
                            for i in 0..high(slots1):
                                if not dynEq(slots1[i], slots2[i]):
                                    cond = false
                                    break outer

            state.stack.add state.makeBool(cond)
        
        else: raise newException(ValueError, "Expected multi kind")
    
    of multikind_include:
        let value2 = state.stack.pop
        let value1 = state.stack.pop

        assert value1.kind == value2.kind #and value1.t == value2.t
        case value1.kind
        of vVMultiKind:
            let table1 = value1.vmk_map
            let table2 = value2.vmk_map

            if table1.len == 0:
                state.stack.add value2
            elif table2.len == 0:
                state.stack.add value1
            else:
                var resTable = table1 # copy
                for key, value in table2.pairs:
                    if not resTable.contains(key):
                        resTable[key] = value
                
                state.stack.add Value(t: value1.t, kind: vVMultiKind, vmk_map: resTable)
        
        of vTMultiKind:
            let table1 = value1.tmk_map
            let table2 = value2.tmk_map

            if table1.len == 0:
                state.stack.add value2
            elif table2.len == 0:
                state.stack.add value1
            else:
                var resTable = table1 # copy
                for key, value in table2.pairs:
                    if not resTable.contains(key):
                        resTable[key] = value
                    elif value != nil:
                        let slots1 = resTable[key][]
                        let slots2 = value[]
                        for i in 0..high(slots1):
                            assert dynEq(slots1[i], slots2[i]), "Not allowed"
                
                state.stack.add Value(t: value1.t, kind: vTMultiKind, tmk_map: resTable)
        
        else: raise newException(ValueError, "Expected multi kind")
    
    of multikind_exclude:
        let value2 = state.stack.pop
        let value1 = state.stack.pop

        assert value1.kind == value2.kind #and value1.t == value2.t
        case value1.kind
        of vVMultiKind:
            let table1 = value1.vmk_map
            let table2 = value2.vmk_map

            if table1.len == 0 or table2.len == 0:
                state.stack.add value1
            else:
                var resTable = table1 # copy
                for key, value in table2.pairs:
                    if resTable.contains(key):
                        resTable.del(key)
                
                state.stack.add Value(t: value1.t, kind: vVMultiKind, vmk_map: resTable)
        
        of vTMultiKind:
            let table1 = value1.tmk_map
            let table2 = value2.tmk_map

            if table1.len == 0 or table2.len == 0:
                state.stack.add value1
            else:
                var resTable = table1 # copy
                for key, value in table2.pairs:
                    if resTable.contains(key):
                        if value == nil:
                            resTable.del(key)
                        else:
                            block check:
                                let slots1 = resTable[key][]
                                let slots2 = value[]
                                for i in 0..high(slots1):
                                    if not dynEq(slots1[i], slots2[i]):
                                        break check
                                resTable.del(key)
                
                state.stack.add Value(t: value1.t, kind: vTMultiKind, tmk_map: resTable)
        
        else: raise newException(ValueError, "Expected multi kind")
    

    # bools
    of bool_not: unaryOp(vBool, not value.b)
    of bool_and: binaryOp(vBool, value1.b and value2.b)
    of bool_or: binaryOp(vBool, value1.b or value2.b)
    of bool_xor: binaryOp(vBool, value1.b xor value2.b)
    of bool_eq: binaryOp(vBool, value1.b == value2.b)
    of bool_ne: binaryOp(vBool, value1.b != value2.b)
    of cast_bool_i8: castOp(vBool, vInt8, value.b)
    of cast_bool_u8: castOp(vBool, vUInt8, value.b)
    of cast_bool_i16: castOp(vBool, vInt16, value.b)
    of cast_bool_u16: castOp(vBool, vUInt16, value.b)
    of cast_bool_i32: castOp(vBool, vInt32, value.b)
    of cast_bool_u32: castOp(vBool, vUInt32, value.b)
    of cast_bool_i64: castOp(vBool, vInt64, value.b)
    of cast_bool_u64: castOp(vBool, vUInt64, value.b)

    # uint8
    of u8_truthy: unaryOp(vUInt8, vBool, value.u8 == 0'u8)
    of u8_compl: unaryOp(vUInt8, not value.u8)
    of u8_succ: unaryOp(vUInt8, value.u8.succ)
    of u8_pred: unaryOp(vUInt8, value.u8.pred)
    of u8_add: binaryOp(vUInt8, value1.u8 + value2.u8)
    of u8_sub: binaryOp(vUInt8, value1.u8 - value2.u8)
    of u8_mult: binaryOp(vUInt8, value1.u8 * value2.u8)
    of u8_pow: binaryOp(vUInt8, value1.u8 ^ value2.u8)
    of u8_div: binaryOp(vUInt8, vDec64, value1.u8.intToDec64 / value2.u8.intToDec64)
    of u8_idiv: binaryOp(vUInt8, value1.u8 div value2.u8)
    of u8_mod: binaryOp(vUInt8, value1.u8 mod value2.u8)
    of u8_mod0: binaryOp(vUInt8, vBool, value1.u8 mod value2.u8 == 0'u8)
    of u8_and: binaryOp(vUInt8, value1.u8 and value2.u8)
    of u8_or: binaryOp(vUInt8, value1.u8 or value2.u8)
    of u8_xor: binaryOp(vUInt8, value1.u8 xor value2.u8)
    of u8_shl: binaryOp(vUInt8, value1.u8 shl value2.u8)
    of u8_shr: binaryOp(vUInt8, value1.u8 shr value2.u8)
    of u8_eq: binaryOp(vUInt8, vBool, value1.u8 == value2.u8)
    of u8_ne: binaryOp(vUInt8, vBool, value1.u8 != value2.u8)
    of u8_gt: binaryOp(vUInt8, vBool, value1.u8 > value2.u8)
    of u8_ge: binaryOp(vUInt8, vBool, value1.u8 >= value2.u8)
    of u8_lt: binaryOp(vUInt8, vBool, value1.u8 < value2.u8)
    of u8_le: binaryOp(vUInt8, vBool, value1.u8 <= value2.u8)
    of cast_u8_bool: castOp(vUInt8, vBool, value.u8)
    of cast_u8_i8: castOp(vUInt8, vInt8, value.u8)
    of cast_u8_i16: castOp(vUInt8, vInt16, value.u8)
    of cast_u8_u16: castOp(vUInt8, vUInt16, value.u8)
    of cast_u8_i32: castOp(vUInt8, vInt32, value.u8)
    of cast_u8_u32: castOp(vUInt8, vUInt32, value.u8)
    of cast_u8_i64: castOp(vUInt8, vInt64, value.u8)
    of cast_u8_u64: castOp(vUInt8, vUInt64, value.u8)
    of cast_u8_f32: castOp(vUInt8, vFloat32, value.u8)
    of cast_u8_f64: castOp(vUInt8, vFloat64, value.u8)
    of cast_u8_d64: castOp(vUInt8, vDec64, value.u8)
    of cast_u8_str: unaryOp(vUInt8, vStr, $value.u8)

    # int8
    of i8_truthy: unaryOp(vInt8, vBool, value.i8 == 0'i8)
    of i8_abs: unaryOp(vInt8, value.i8.abs)
    of i8_neg: unaryOp(vInt8, -value.i8)
    of i8_compl: unaryOp(vInt8, not value.i8)
    of i8_succ: unaryOp(vInt8, value.i8.succ)
    of i8_pred: unaryOp(vInt8, value.i8.pred)
    of i8_add: binaryOp(vInt8, value1.i8 + value2.i8)
    of i8_sub: binaryOp(vInt8, value1.i8 - value2.i8)
    of i8_mult: binaryOp(vInt8, value1.i8 * value2.i8)
    of i8_pow: binaryOp(vInt8, value1.i8 ^ value2.i8)
    of i8_div: binaryOp(vInt8, vDec64, value1.i8.intToDec64 / value2.i8.intToDec64)
    of i8_idiv: binaryOp(vInt8, value1.i8 div value2.i8)
    of i8_mod: binaryOp(vInt8, value1.i8 mod value2.i8)
    of i8_mod0: binaryOp(vInt8, vBool, value1.i8 mod value2.i8 == 0'i8)
    of i8_and: binaryOp(vInt8, value1.i8 and value2.i8)
    of i8_or: binaryOp(vInt8, value1.i8 or value2.i8)
    of i8_xor: binaryOp(vInt8, value1.i8 xor value2.i8)
    of i8_shl: binaryOp(vInt8, value1.i8 shl value2.i8)
    of i8_shr: binaryOp(vInt8, value1.i8 shr value2.i8)
    of i8_eq: binaryOp(vInt8, vBool, value1.i8 == value2.i8)
    of i8_ne: binaryOp(vInt8, vBool, value1.i8 != value2.i8)
    of i8_gt: binaryOp(vInt8, vBool, value1.i8 > value2.i8)
    of i8_ge: binaryOp(vInt8, vBool, value1.i8 >= value2.i8)
    of i8_lt: binaryOp(vInt8, vBool, value1.i8 < value2.i8)
    of i8_le: binaryOp(vInt8, vBool, value1.i8 <= value2.i8)
    of cast_i8_bool: castOp(vInt8, vBool, value.i8)
    of cast_i8_u8: castOp(vInt8, vUInt8, value.i8)
    of cast_i8_i16: castOp(vInt8, vInt16, value.i8)
    of cast_i8_u16: castOp(vInt8, vUInt16, value.i8)
    of cast_i8_i32: castOp(vInt8, vInt32, value.i8)
    of cast_i8_u32: castOp(vInt8, vUInt32, value.i8)
    of cast_i8_i64: castOp(vInt8, vInt64, value.i8)
    of cast_i8_u64: castOp(vInt8, vUInt64, value.i8)
    of cast_i8_f32: castOp(vInt8, vFloat32, value.i8)
    of cast_i8_f64: castOp(vInt8, vFloat64, value.i8)
    of cast_i8_d64: castOp(vInt8, vDec64, value.i8)
    of cast_i8_str: unaryOp(vInt8, vStr, $value.i8)

    # uint16
    of u16_truthy: unaryOp(vUInt16, vBool, value.u16 == 0'u16)
    of u16_compl: unaryOp(vUInt16, not value.u16)
    of u16_succ: unaryOp(vUInt16, value.u16.succ)
    of u16_pred: unaryOp(vUInt16, value.u16.pred)
    of u16_add: binaryOp(vUInt16, value1.u16 + value2.u16)
    of u16_sub: binaryOp(vUInt16, value1.u16 - value2.u16)
    of u16_mult: binaryOp(vUInt16, value1.u16 * value2.u16)
    of u16_pow: binaryOp(vUInt16, value1.u16 ^ value2.u16)
    of u16_div: binaryOp(vUInt16, vDec64, value1.u16.intToDec64 / value2.u16.intToDec64)
    of u16_idiv: binaryOp(vUInt16, value1.u16 div value2.u16)
    of u16_mod: binaryOp(vUInt16, value1.u16 mod value2.u16)
    of u16_mod0: binaryOp(vUInt16, vBool, value1.u16 mod value2.u16 == 0'u16)
    of u16_and: binaryOp(vUInt16, value1.u16 and value2.u16)
    of u16_or: binaryOp(vUInt16, value1.u16 or value2.u16)
    of u16_xor: binaryOp(vUInt16, value1.u16 xor value2.u16)
    of u16_shl: binaryOp(vUInt16, value1.u16 shl value2.u16)
    of u16_shr: binaryOp(vUInt16, value1.u16 shr value2.u16)
    of u16_eq: binaryOp(vUInt16, vBool, value1.u16 == value2.u16)
    of u16_ne: binaryOp(vUInt16, vBool, value1.u16 != value2.u16)
    of u16_gt: binaryOp(vUInt16, vBool, value1.u16 > value2.u16)
    of u16_ge: binaryOp(vUInt16, vBool, value1.u16 >= value2.u16)
    of u16_lt: binaryOp(vUInt16, vBool, value1.u16 < value2.u16)
    of u16_le: binaryOp(vUInt16, vBool, value1.u16 <= value2.u16)
    of cast_u16_bool: castOp(vUInt16, vBool, value.u16)
    of cast_u16_i8: castOp(vUInt16, vInt8, value.u16)
    of cast_u16_u8: castOp(vUInt16, vUInt8, value.u16)
    of cast_u16_i16: castOp(vUInt16, vInt16, value.u16)
    of cast_u16_i32: castOp(vUInt16, vInt32, value.u16)
    of cast_u16_u32: castOp(vUInt16, vUInt32, value.u16)
    of cast_u16_i64: castOp(vUInt16, vInt64, value.u16)
    of cast_u16_u64: castOp(vUInt16, vUInt64, value.u16)
    of cast_u16_f32: castOp(vUInt16, vFloat32, value.u16)
    of cast_u16_f64: castOp(vUInt16, vFloat64, value.u16)
    of cast_u16_d64: castOp(vUInt16, vDec64, value.u16)
    of cast_u16_str: unaryOp(vUInt16, vStr, $value.u16)

    # int16
    of i16_truthy: unaryOp(vInt16, vBool, value.i16 == 0'i16)
    of i16_abs: unaryOp(vInt16, value.i16.abs)
    of i16_neg: unaryOp(vInt16, -value.i16)
    of i16_compl: unaryOp(vInt16, not value.i16)
    of i16_succ: unaryOp(vInt16, value.i16.succ)
    of i16_pred: unaryOp(vInt16, value.i16.pred)
    of i16_add: binaryOp(vInt16, value1.i16 + value2.i16)
    of i16_sub: binaryOp(vInt16, value1.i16 - value2.i16)
    of i16_mult: binaryOp(vInt16, value1.i16 * value2.i16)
    of i16_pow: binaryOp(vInt16, value1.i16 ^ value2.i16)
    of i16_div: binaryOp(vInt16, vDec64, value1.i16.intToDec64 / value2.i16.intToDec64)
    of i16_idiv: binaryOp(vInt16, value1.i16 div value2.i16)
    of i16_mod: binaryOp(vInt16, value1.i16 mod value2.i16)
    of i16_mod0: binaryOp(vInt16, vBool, value1.i16 mod value2.i16 == 0'i16)
    of i16_and: binaryOp(vInt16, value1.i16 and value2.i16)
    of i16_or: binaryOp(vInt16, value1.i16 or value2.i16)
    of i16_xor: binaryOp(vInt16, value1.i16 xor value2.i16)
    of i16_shl: binaryOp(vInt16, value1.i16 shl value2.i16)
    of i16_shr: binaryOp(vInt16, value1.i16 shr value2.i16)
    of i16_eq: binaryOp(vInt16, vBool, value1.i16 == value2.i16)
    of i16_ne: binaryOp(vInt16, vBool, value1.i16 != value2.i16)
    of i16_gt: binaryOp(vInt16, vBool, value1.i16 > value2.i16)
    of i16_ge: binaryOp(vInt16, vBool, value1.i16 >= value2.i16)
    of i16_lt: binaryOp(vInt16, vBool, value1.i16 < value2.i16)
    of i16_le: binaryOp(vInt16, vBool, value1.i16 <= value2.i16)
    of cast_i16_bool: castOp(vInt16, vBool, value.i16)
    of cast_i16_i8: castOp(vInt16, vInt8, value.i16)
    of cast_i16_u8: castOp(vInt16, vUInt8, value.i16)
    of cast_i16_u16: castOp(vInt16, vUInt16, value.i16)
    of cast_i16_i32: castOp(vInt16, vInt32, value.i16)
    of cast_i16_u32: castOp(vInt16, vUInt32, value.i16)
    of cast_i16_i64: castOp(vInt16, vInt64, value.i16)
    of cast_i16_u64: castOp(vInt16, vUInt64, value.i16)
    of cast_i16_f32: castOp(vInt16, vFloat32, value.i16)
    of cast_i16_f64: castOp(vInt16, vFloat64, value.i16)
    of cast_i16_d64: castOp(vInt16, vDec64, value.i16)
    of cast_i16_str: unaryOp(vInt16, vStr, $value.i16)

    # uint32
    of u32_truthy: unaryOp(vUInt32, vBool, value.u32 == 0'u32)
    of u32_compl: unaryOp(vUInt32, not value.u32)
    of u32_succ: unaryOp(vUInt32, value.u32.succ)
    of u32_pred: unaryOp(vUInt32, value.u32.pred)
    of u32_add: binaryOp(vUInt32, value1.u32 + value2.u32)
    of u32_sub: binaryOp(vUInt32, value1.u32 - value2.u32)
    of u32_mult: binaryOp(vUInt32, value1.u32 * value2.u32)
    of u32_pow: binaryOp(vUInt32, value1.u32 ^ value2.u32)
    of u32_div: binaryOp(vUInt32, vDec64, value1.u32.intToDec64 / value2.u32.intToDec64)
    of u32_idiv: binaryOp(vUInt32, value1.u32 div value2.u32)
    of u32_mod: binaryOp(vUInt32, value1.u32 mod value2.u32)
    of u32_mod0: binaryOp(vUInt32, vBool, value1.u32 mod value2.u32 == 0'u32)
    of u32_and: binaryOp(vUInt32, value1.u32 and value2.u32)
    of u32_or: binaryOp(vUInt32, value1.u32 or value2.u32)
    of u32_xor: binaryOp(vUInt32, value1.u32 xor value2.u32)
    of u32_shl: binaryOp(vUInt32, value1.u32 shl value2.u32)
    of u32_shr: binaryOp(vUInt32, value1.u32 shr value2.u32)
    of u32_eq: binaryOp(vUInt32, vBool, value1.u32 == value2.u32)
    of u32_ne: binaryOp(vUInt32, vBool, value1.u32 != value2.u32)
    of u32_gt: binaryOp(vUInt32, vBool, value1.u32 > value2.u32)
    of u32_ge: binaryOp(vUInt32, vBool, value1.u32 >= value2.u32)
    of u32_lt: binaryOp(vUInt32, vBool, value1.u32 < value2.u32)
    of u32_le: binaryOp(vUInt32, vBool, value1.u32 <= value2.u32)
    of cast_u32_bool: castOp(vUInt32, vBool, value.u32)
    of cast_u32_i8: castOp(vUInt32, vInt8, value.u32)
    of cast_u32_u8: castOp(vUInt32, vUInt8, value.u32)
    of cast_u32_i16: castOp(vUInt32, vInt16, value.u32)
    of cast_u32_u16: castOp(vUInt32, vUInt16, value.u32)
    of cast_u32_i32: castOp(vUInt32, vInt32, value.u32)
    of cast_u32_i64: castOp(vUInt32, vInt64, value.u32)
    of cast_u32_u64: castOp(vUInt32, vUInt64, value.u32)
    of cast_u32_f32: castOp(vUInt32, vFloat32, value.u32)
    of cast_u32_f64: castOp(vUInt32, vFloat64, value.u32)
    of cast_u32_d64: castOp(vUInt32, vDec64, value.u32)
    of cast_u32_str: unaryOp(vUInt32, vStr, $value.u32)

    # int32
    of i32_truthy: unaryOp(vInt32, vBool, value.i32 == 0'i32)
    of i32_abs: unaryOp(vInt32, value.i32.abs)
    of i32_neg: unaryOp(vInt32, -value.i32)
    of i32_compl: unaryOp(vInt32, not value.i32)
    of i32_succ: unaryOp(vInt32, value.i32.succ)
    of i32_pred: unaryOp(vInt32, value.i32.pred)
    of i32_add: binaryOp(vInt32, value1.i32 + value2.i32)
    of i32_sub: binaryOp(vInt32, value1.i32 - value2.i32)
    of i32_mult: binaryOp(vInt32, value1.i32 * value2.i32)
    of i32_pow: binaryOp(vInt32, value1.i32 ^ value2.i32)
    of i32_div: binaryOp(vInt32, vDec64, value1.i32.intToDec64 / value2.i32.intToDec64)
    of i32_idiv: binaryOp(vInt32, value1.i32 div value2.i32)
    of i32_mod: binaryOp(vInt32, value1.i32 mod value2.i32)
    of i32_mod0: binaryOp(vInt32, vBool, value1.i32 mod value2.i32 == 0'i32)
    of i32_and: binaryOp(vInt32, value1.i32 and value2.i32)
    of i32_or: binaryOp(vInt32, value1.i32 or value2.i32)
    of i32_xor: binaryOp(vInt32, value1.i32 xor value2.i32)
    of i32_shl: binaryOp(vInt32, value1.i32 shl value2.i32)
    of i32_shr: binaryOp(vInt32, value1.i32 shr value2.i32)
    of i32_eq: binaryOp(vInt32, vBool, value1.i32 == value2.i32)
    of i32_ne: binaryOp(vInt32, vBool, value1.i32 != value2.i32)
    of i32_gt: binaryOp(vInt32, vBool, value1.i32 > value2.i32)
    of i32_ge: binaryOp(vInt32, vBool, value1.i32 >= value2.i32)
    of i32_lt: binaryOp(vInt32, vBool, value1.i32 < value2.i32)
    of i32_le: binaryOp(vInt32, vBool, value1.i32 <= value2.i32)
    of cast_i32_bool: castOp(vInt32, vBool, value.i32)
    of cast_i32_i8: castOp(vInt32, vInt8, value.i32)
    of cast_i32_u8: castOp(vInt32, vUInt8, value.i32)
    of cast_i32_i16: castOp(vInt32, vInt16, value.u32)
    of cast_i32_u16: castOp(vInt32, vUInt16, value.i32)
    of cast_i32_u32: castOp(vInt32, vUInt32, value.i32)
    of cast_i32_i64: castOp(vInt32, vInt64, value.i32)
    of cast_i32_u64: castOp(vInt32, vUInt64, value.i32)
    of cast_i32_f32: castOp(vInt32, vFloat32, value.i32)
    of cast_i32_f64: castOp(vInt32, vFloat64, value.i32)
    of cast_i32_d64: castOp(vInt32, vDec64, value.i32)
    of cast_i32_str: unaryOp(vInt32, vStr, $value.i32)


    # uint64
    of u64_truthy: unaryOp(vUInt64, vBool, value.u64 == 0'u64)
    of u64_compl: unaryOp(vUInt64, not value.u64)
    of u64_succ: unaryOp(vUInt64, value.u64.succ)
    of u64_pred: unaryOp(vUInt64, value.u64.pred)
    of u64_add: binaryOp(vUInt64, value1.u64 + value2.u64)
    of u64_sub: binaryOp(vUInt64, value1.u64 - value2.u64)
    of u64_mult: binaryOp(vUInt64, value1.u64 * value2.u64)
    of u64_pow: binaryOp(vUInt64, value1.u64 ^ value2.u64)
    of u64_div: binaryOp(vUInt64, vDec64, value1.u64.intToDec64 / value2.u64.intToDec64)
    of u64_idiv: binaryOp(vUInt64, value1.u64 div value2.u64)
    of u64_mod: binaryOp(vUInt64, value1.u64 mod value2.u64)
    of u64_mod0: binaryOp(vUInt64, vBool, value1.u64 mod value2.u64 == 0'u64)
    of u64_and: binaryOp(vUInt64, value1.u64 and value2.u64)
    of u64_or: binaryOp(vUInt64, value1.u64 or value2.u64)
    of u64_xor: binaryOp(vUInt64, value1.u64 xor value2.u64)
    of u64_shl: binaryOp(vUInt64, value1.u64 shl value2.u64)
    of u64_shr: binaryOp(vUInt64, value1.u64 shr value2.u64)
    of u64_eq: binaryOp(vUInt64, vBool, value1.u64 == value2.u64)
    of u64_ne: binaryOp(vUInt64, vBool, value1.u64 != value2.u64)
    of u64_gt: binaryOp(vUInt64, vBool, value1.u64 > value2.u64)
    of u64_ge: binaryOp(vUInt64, vBool, value1.u64 >= value2.u64)
    of u64_lt: binaryOp(vUInt64, vBool, value1.u64 < value2.u64)
    of u64_le: binaryOp(vUInt64, vBool, value1.u64 <= value2.u64)
    of cast_u64_bool: castOp(vUInt64, vBool, value.u64)
    of cast_u64_i8: castOp(vUInt64, vInt8, value.u64)
    of cast_u64_u8: castOp(vUInt64, vUInt8, value.u64)
    of cast_u64_i16: castOp(vUInt64, vInt16, value.u64)
    of cast_u64_u16: castOp(vUInt64, vUInt16, value.u64)
    of cast_u64_i32: castOp(vUInt64, vInt32, value.u64)
    of cast_u64_u32: castOp(vUInt64, vUInt32, value.u64)
    of cast_u64_i64: castOp(vUInt64, vInt64, value.u64)
    of cast_u64_f32: castOp(vUInt64, vFloat32, value.u64)
    of cast_u64_f64: castOp(vUInt64, vFloat64, value.u64)
    of cast_u64_d64: castOp(vUInt64, vDec64, value.u64)
    of cast_u64_str: unaryOp(vUInt64, vStr, $value.u64)

    # int64
    of i64_truthy: unaryOp(vInt64, vBool, value.i64 == 0'i64)
    of i64_abs: unaryOp(vInt64, value.i64.abs)
    of i64_neg: unaryOp(vInt64, -value.i64)
    of i64_compl: unaryOp(vInt64, not value.i64)
    of i64_succ: unaryOp(vInt64, value.i64.succ)
    of i64_pred: unaryOp(vInt64, value.i64.pred)
    of i64_add: binaryOp(vInt64, value1.i64 + value2.i64)
    of i64_sub: binaryOp(vInt64, value1.i64 - value2.i64)
    of i64_mult: binaryOp(vInt64, value1.i64 * value2.i64)
    of i64_pow: binaryOp(vInt64, value1.i64 ^ value2.i64)
    of i64_div: binaryOp(vInt64, vDec64, value1.i64.intToDec64 / value2.i64.intToDec64)
    of i64_idiv: binaryOp(vInt64, value1.i64 div value2.i64)
    of i64_mod: binaryOp(vInt64, value1.i64 mod value2.i64)
    of i64_mod0: binaryOp(vInt64, vBool, value1.i64 mod value2.i64 == 0'i64)
    of i64_and: binaryOp(vInt64, value1.i64 and value2.i64)
    of i64_or: binaryOp(vInt64, value1.i64 or value2.i64)
    of i64_xor: binaryOp(vInt64, value1.i64 xor value2.i64)
    of i64_shl: binaryOp(vInt64, value1.i64 shl value2.i64)
    of i64_shr: binaryOp(vInt64, value1.i64 shr value2.i64)
    of i64_eq: binaryOp(vInt64, vBool, value1.i64 == value2.i64)
    of i64_ne: binaryOp(vInt64, vBool, value1.i64 != value2.i64)
    of i64_gt: binaryOp(vInt64, vBool, value1.i64 > value2.i64)
    of i64_ge: binaryOp(vInt64, vBool, value1.i64 >= value2.i64)
    of i64_lt: binaryOp(vInt64, vBool, value1.i64 < value2.i64)
    of i64_le: binaryOp(vInt64, vBool, value1.i64 <= value2.i64)
    of cast_i64_bool: castOp(vInt64, vBool, value.i64)
    of cast_i64_i8: castOp(vInt64, vInt8, value.i64)
    of cast_i64_u8: castOp(vInt64, vUInt8, value.i64)
    of cast_i64_i16: castOp(vInt64, vInt16, value.u64)
    of cast_i64_u16: castOp(vInt64, vUInt16, value.i64)
    of cast_i64_i32: castOp(vInt64, vInt32, value.i64)
    of cast_i64_u32: castOp(vInt64, vUInt32, value.i64)
    of cast_i64_u64: castOp(vInt64, vUInt64, value.i64)
    of cast_i64_f32: castOp(vInt64, vFloat32, value.i64)
    of cast_i64_f64: castOp(vInt64, vFloat64, value.i64)
    of cast_i64_d64: castOp(vInt64, vDec64, value.i64)
    of cast_i64_str: unaryOp(vInt64, vStr, $value.i64)

    # float32
    of f32_nan: state.stack.add state.makeFloat32(NaN32)
    of f32_inf: state.stack.add state.makeFloat32(Inf32)
    of f32_neg_inf: state.stack.add state.makeFloat32(NegInf32)
    of f32_pi: state.stack.add state.makeFloat32(PI.float32)
    of f32_e: state.stack.add state.makeFloat32(E)
    of f32_is_nan: unaryOp(vFloat32, vBool, value.f32.isNaN)
    of f32_sign: unaryOp(vFloat32, value.f32.sgn.float32)
    of f32_abs: unaryOp(vFloat32, value.f32.abs)
    of f32_sqrt: unaryOp(vFloat32, value.f32.sqrt)
    of f32_exp: unaryOp(vFloat32, value.f32.exp)
    of f32_sin: unaryOp(vFloat32, value.f32.sin)
    of f32_cos: unaryOp(vFloat32, value.f32.cos)
    of f32_tan: unaryOp(vFloat32, value.f32.tan)
    of f32_asin: unaryOp(vFloat32, value.f32.arcsin)
    of f32_acos: unaryOp(vFloat32, value.f32.arccos)
    of f32_atan: unaryOp(vFloat32, value.f32.arctan)
    of f32_floor: unaryOp(vFloat32, value.f32.floor)
    of f32_ceil: unaryOp(vFloat32, value.f32.ceil)
    of f32_trunc: unaryOp(vFloat32, value.f32.trunc)
    of f32_round: unaryOp(vFloat32, value.f32.round)
    of f32_ln: unaryOp(vFloat32, value.f32.ln)
    of f32_log: unaryOp(vFloat32, value.f32.log10)
    of f32_truthy: unaryOp(vFloat32, vBool, value.f32 == 0'f32)
    of f32_neg: unaryOp(vFloat32, -value.f32)
    of f32_succ: unaryOp(vFloat32, value.f32 + 1'f32)
    of f32_pred: unaryOp(vFloat32, value.f32 - 1'f32)
    of f32_add: binaryOp(vFloat32, value1.f32 + value2.f32)
    of f32_sub: binaryOp(vFloat32, value1.f32 - value2.f32)
    of f32_mult: binaryOp(vFloat32, value1.f32 * value2.f32)
    of f32_pow: binaryOp(vFloat32, pow(value1.f32, value2.f32))
    of f32_div: binaryOp(vFloat32, value1.f32 / value2.f32)
    of f32_idiv: binaryOp(vFloat32, vInt32, int32(value1.f32 / value2.f32))
    of f32_mod: binaryOp(vFloat32, value1.f32 mod value2.f32)
    of f32_mod0: binaryOp(vFloat32, vBool, value1.f32 mod value2.f32 == 0'f32)
    of f32_eq: binaryOp(vFloat32, vBool, value1.f32 == value2.f32)
    of f32_ne: binaryOp(vFloat32, vBool, value1.f32 != value2.f32)
    of f32_gt: binaryOp(vFloat32, vBool, value1.f32 > value2.f32)
    of f32_ge: binaryOp(vFloat32, vBool, value1.f32 >= value2.f32)
    of f32_lt: binaryOp(vFloat32, vBool, value1.f32 < value2.f32)
    of f32_le: binaryOp(vFloat32, vBool, value1.f32 <= value2.f32)
    of cast_f32_i8: castOp(vFloat32, vInt8, value.f32)
    of cast_f32_u8: castOp(vFloat32, vUInt8, value.f32)
    of cast_f32_i16: castOp(vFloat32, vInt16, value.f32)
    of cast_f32_u16: castOp(vFloat32, vUInt16, value.f32)
    of cast_f32_i32: castOp(vFloat32, vInt32, value.f32)
    of cast_f32_u32: castOp(vFloat32, vUInt32, value.f32)
    of cast_f32_i64: castOp(vFloat32, vInt64, value.f32)
    of cast_f32_u64: castOp(vFloat32, vUInt64, value.f32)
    of cast_f32_f64: castOp(vFloat32, vFloat64, value.f32)
    of cast_f32_d64: raiseAssert "TODO"
    of cast_f32_str: unaryOp(vFloat32, vStr, $value.f32)

    # float64
    of f64_nan: state.stack.add state.makeFloat64(NaN)
    of f64_inf: state.stack.add state.makeFloat64(Inf)
    of f64_neg_inf: state.stack.add state.makeFloat64(NegInf)
    of f64_pi: state.stack.add state.makeFloat64(PI)
    of f64_e: state.stack.add state.makeFloat64(E)
    of f64_is_nan: unaryOp(vFloat64, vBool, value.f64.isNaN)
    of f64_sign: unaryOp(vFloat64, value.f64.sgn.float64)
    of f64_abs: unaryOp(vFloat64, value.f64.abs)
    of f64_sqrt: unaryOp(vFloat64, value.f64.sqrt)
    of f64_exp: unaryOp(vFloat64, value.f64.exp)
    of f64_sin: unaryOp(vFloat64, value.f64.sin)
    of f64_cos: unaryOp(vFloat64, value.f64.cos)
    of f64_tan: unaryOp(vFloat64, value.f64.tan)
    of f64_asin: unaryOp(vFloat64, value.f64.arcsin)
    of f64_acos: unaryOp(vFloat64, value.f64.arccos)
    of f64_atan: unaryOp(vFloat64, value.f64.arctan)
    of f64_floor: unaryOp(vFloat64, value.f64.floor)
    of f64_ceil: unaryOp(vFloat64, value.f64.ceil)
    of f64_trunc: unaryOp(vFloat64, value.f64.trunc)
    of f64_round: unaryOp(vFloat64, value.f64.round)
    of f64_ln: unaryOp(vFloat64, value.f64.ln)
    of f64_log: unaryOp(vFloat64, value.f64.log10)
    of f64_truthy: unaryOp(vFloat64, vBool, value.f64 == 0'f64)
    of f64_neg: unaryOp(vFloat64, -value.f64)
    of f64_succ: unaryOp(vFloat64, value.f64 + 1'f64)
    of f64_pred: unaryOp(vFloat64, value.f64 - 1'f64)
    of f64_add: binaryOp(vFloat64, value1.f64 + value2.f64)
    of f64_sub: binaryOp(vFloat64, value1.f64 - value2.f64)
    of f64_mult: binaryOp(vFloat64, value1.f64 * value2.f64)
    of f64_pow: binaryOp(vFloat64, pow(value1.f64, value2.f64))
    of f64_div: binaryOp(vFloat64, value1.f64 / value2.f64)
    of f64_idiv: binaryOp(vFloat64, vInt32, int32(value1.f64 / value2.f64))
    of f64_mod: binaryOp(vFloat64, value1.f64 mod value2.f64)
    of f64_mod0: binaryOp(vFloat64, vBool, value1.f64 mod value2.f64 == 0'f64)
    of f64_eq: binaryOp(vFloat64, vBool, value1.f64 == value2.f64)
    of f64_ne: binaryOp(vFloat64, vBool, value1.f64 != value2.f64)
    of f64_gt: binaryOp(vFloat64, vBool, value1.f64 > value2.f64)
    of f64_ge: binaryOp(vFloat64, vBool, value1.f64 >= value2.f64)
    of f64_lt: binaryOp(vFloat64, vBool, value1.f64 < value2.f64)
    of f64_le: binaryOp(vFloat64, vBool, value1.f64 <= value2.f64)
    of cast_f64_i8: castOp(vFloat64, vInt8, value.f64)
    of cast_f64_u8: castOp(vFloat64, vUInt8, value.f64)
    of cast_f64_i16: castOp(vFloat64, vInt16, value.f64)
    of cast_f64_u16: castOp(vFloat64, vUInt16, value.f64)
    of cast_f64_i32: castOp(vFloat64, vInt32, value.f64)
    of cast_f64_u32: castOp(vFloat64, vUInt32, value.f64)
    of cast_f64_i64: castOp(vFloat64, vInt64, value.f64)
    of cast_f64_u64: castOp(vFloat64, vUInt64, value.f64)
    of cast_f64_f32: castOp(vFloat64, vFloat32, value.f64)
    of cast_f64_d64: raiseAssert "TODO"
    of cast_f64_str: unaryOp(vFloat64, vStr, $value.f64)

    # dec64
    of d64_new:
        let value2 = state.stack.pop; assert value2.kind == vInt8, "Expected uint8"
        let value1 = state.stack.pop; assert value1.kind == vInt64, "Expected int64"
        state.stack.add state.makeDec64(makeDec64(value1.i64, value2.i8))
    of d64_get_coef: unaryOp(vDec64, vInt64, value.d64.coefficient)
    of d64_get_exp: unaryOp(vDec64, vInt8, value.d64.exponent)
    of d64_nan: state.stack.add state.makeDec64(DEC64_NAN)
    of d64_pi: state.stack.add state.makeDec64(DEC64_PI)
    of d64_e: state.stack.add state.makeDec64(DEC64_E)
    of d64_is_nan: unaryOp(vDec64, vBool, value.d64.is_nan)
    of d64_sign: unaryOp(vDec64, value.d64.signum)
    of d64_abs: unaryOp(vDec64, value.d64.abs)
    of d64_sqrt: unaryOp(vDec64, value.d64.sqrt)
    of d64_exp: unaryOp(vDec64, value.d64.exp)
    of d64_sin: unaryOp(vDec64, value.d64.sin)
    of d64_cos: unaryOp(vDec64, value.d64.cos)
    of d64_tan: unaryOp(vDec64, value.d64.tan)
    of d64_asin: unaryOp(vDec64, value.d64.asin)
    of d64_acos: unaryOp(vDec64, value.d64.acos)
    of d64_atan: unaryOp(vDec64, value.d64.atan)
    of d64_floor: unaryOp(vDec64, value.d64.floor)
    of d64_ceil: unaryOp(vDec64, value.d64.ceil)
    of d64_trunc: unaryOp(vDec64, value.d64.trunc)
    of d64_round: unaryOp(vDec64, value.d64.round(DEC64_ZERO))
    of d64_ln: unaryOp(vDec64, value.d64.log / DEC64_E.log)
    of d64_log: unaryOp(vDec64, value.d64.log)
    of d64_normal: unaryOp(vDec64, value.d64.normal)
    of d64_truthy: unaryOp(vDec64, vBool, value.d64.is_zero)
    of d64_neg: unaryOp(vDec64, -value.d64)
    of d64_succ: unaryOp(vDec64, value.d64 + DEC64_ONE)
    of d64_pred: unaryOp(vDec64, value.d64 + DEC64_NEG_ONE)
    of d64_add: binaryOp(vDec64, value1.d64 + value2.d64)
    of d64_sub: binaryOp(vDec64, value1.d64 - value2.d64)
    of d64_mult: binaryOp(vDec64, value1.d64 * value2.d64)
    of d64_pow: binaryOp(vDec64, pow(value1.d64, value2.d64))
    of d64_div: binaryOp(vDec64, value1.d64 / value2.d64)
    of d64_idiv: binaryOp(vDec64, vInt32, dec64ToInt[int32](intDiv(value1.d64, value2.d64)))
    of d64_mod: binaryOp(vDec64, value1.d64 mod value2.d64)
    of d64_mod0: binaryOp(vDec64, vBool, is_zero(value1.d64 mod value2.d64))
    of d64_eq: binaryOp(vDec64, vBool, value1.d64 == value2.d64)
    of d64_ne: binaryOp(vDec64, vBool, value1.d64 != value2.d64)
    of d64_gt: binaryOp(vDec64, vBool, value1.d64 > value2.d64)
    of d64_ge: binaryOp(vDec64, vBool, value1.d64 >= value2.d64)
    of d64_lt: binaryOp(vDec64, vBool, value1.d64 < value2.d64)
    of d64_le: binaryOp(vDec64, vBool, value1.d64 <= value2.d64)
    of cast_d64_i8: raiseAssert "TODO"
    of cast_d64_u8: raiseAssert "TODO"
    of cast_d64_i16: raiseAssert "TODO"
    of cast_d64_u16: raiseAssert "TODO"
    of cast_d64_i32: raiseAssert "TODO"
    of cast_d64_u32: raiseAssert "TODO"
    of cast_d64_i64: raiseAssert "TODO"
    of cast_d64_u64: raiseAssert "TODO"
    of cast_d64_f32: raiseAssert "TODO"
    of cast_d64_f64: raiseAssert "TODO"
    of cast_d64_str: unaryOp(vFloat32, vStr, $value.d64)

    # ptr
    #of ptr_new:
    #    discard # TODO: didn't think this one out very well
    
    of ptr_get_deref:
        let pvalue = state.stack.pop; assert likely(pvalue.kind == vPtr), "Expected a typed pointer"
        state.stack.add pvalue.`ptr`[pvalue.offset]
    
    of ptr_set_deref:
        let value = state.stack.pop
        let pvalue = state.stack.pop; assert likely(pvalue.kind == vPtr), "Expected a typed pointer"
        pvalue.`ptr`[pvalue.offset] = value
    
    of ptr_get_at:
        let pos = state.stack.pop; assert likely(pos.kind in {vInt8..vUInt64}), "Expected an integer"
        let pvalue = state.stack.pop; assert likely(pvalue.kind == vPtr), "Expected a typed pointer"
        let offset =
            case pos.kind
            of vInt8: pvalue.offset + pos.i8.uint
            of vUInt8: pvalue.offset + pos.u8.uint
            of vInt16: pvalue.offset + pos.i16.uint
            of vUInt16: pvalue.offset + pos.u16.uint
            of vInt32: pvalue.offset + pos.i32.uint
            of vUInt32: pvalue.offset + pos.u32.uint
            of vInt64: pvalue.offset + pos.i64.uint
            of vUInt64: pvalue.offset + pos.u64.uint
            else: 0 # impossible
        
        state.stack.add pvalue.`ptr`[offset]
    
    of ptr_set_at:
        let value = state.stack.pop
        let pos = state.stack.pop; assert likely(pos.kind in {vInt8..vUInt64}), "Expected an integer"
        let pvalue = state.stack.pop; assert likely(pvalue.kind == vPtr), "Expected a typed pointer"
        let offset =
            case pos.kind
            of vInt8: pvalue.offset + pos.i8.uint
            of vUInt8: pvalue.offset + pos.u8.uint
            of vInt16: pvalue.offset + pos.i16.uint
            of vUInt16: pvalue.offset + pos.u16.uint
            of vInt32: pvalue.offset + pos.i32.uint
            of vUInt32: pvalue.offset + pos.u32.uint
            of vInt64: pvalue.offset + pos.i64.uint
            of vUInt64: pvalue.offset + pos.u64.uint
            else: 0 # impossible
        
        pvalue.`ptr`[offset] = value
    
    of ptr_add:
        let by = state.stack.pop; assert likely(by.kind in {vInt8..vUInt64}), "Expected an integer"
        let pvalue = state.stack.pop

        case pvalue.kind
        of vPtr:
            let offset =
                case by.kind
                of vInt8: pvalue.offset + by.i8.uint
                of vUInt8: pvalue.offset + by.u8.uint
                of vInt16: pvalue.offset + by.i16.uint
                of vUInt16: pvalue.offset + by.u16.uint
                of vInt32: pvalue.offset + by.i32.uint
                of vUInt32: pvalue.offset + by.u32.uint
                of vInt64: pvalue.offset + by.i64.uint
                of vUInt64: pvalue.offset + by.u64.uint
                else: 0 # impossible
            
            state.stack.add Value(t: pvalue.t, kind: vPtr, offset: offset, `ptr`: pvalue.ptr)
        of vVoidPtr:
            let newPtr =
                case by.kind
                of vInt8: cast[pointer](cast[uint64](pvalue.vptr) + by.i8.uint64)
                of vUInt8: cast[pointer](cast[uint64](pvalue.vptr) + by.u8.uint64)
                of vInt16: cast[pointer](cast[uint64](pvalue.vptr) + by.i16.uint64)
                of vUInt16: cast[pointer](cast[uint64](pvalue.vptr) + by.u16.uint64)
                of vInt32: cast[pointer](cast[uint64](pvalue.vptr) + by.i32.uint64)
                of vUInt32: cast[pointer](cast[uint64](pvalue.vptr) + by.u32.uint64)
                of vInt64: cast[pointer](cast[uint64](pvalue.vptr) + by.i64.uint64)
                of vUInt64: cast[pointer](cast[uint64](pvalue.vptr) + by.u64.uint64)
                else: nil # impossible
            
            state.stack.add Value(t: pvalue.t, kind: vVoidPtr, vptr: newPtr)
        else:
            raise newException(ValueError, "Expected a pointer")
    
    of ptr_sub:
        let by = state.stack.pop; assert likely(by.kind in {vInt8..vUInt64}), "Expected an integer"
        let pvalue = state.stack.pop
        
        case pvalue.kind
        of vPtr:
            let offset =
                case by.kind
                of vInt8: pvalue.offset - by.i8.uint
                of vUInt8: pvalue.offset - by.u8.uint
                of vInt16: pvalue.offset - by.i16.uint
                of vUInt16: pvalue.offset - by.u16.uint
                of vInt32: pvalue.offset - by.i32.uint
                of vUInt32: pvalue.offset - by.u32.uint
                of vInt64: pvalue.offset - by.i64.uint
                of vUInt64: pvalue.offset - by.u64.uint
                else: 0 # impossible
            
            state.stack.add Value(t: pvalue.t, kind: vPtr, offset: offset, `ptr`: pvalue.ptr)
        of vVoidPtr:
            let newPtr =
                case by.kind
                of vInt8: cast[pointer](cast[uint64](pvalue.vptr) - by.i8.uint64)
                of vUInt8: cast[pointer](cast[uint64](pvalue.vptr) - by.u8.uint64)
                of vInt16: cast[pointer](cast[uint64](pvalue.vptr) - by.i16.uint64)
                of vUInt16: cast[pointer](cast[uint64](pvalue.vptr) - by.u16.uint64)
                of vInt32: cast[pointer](cast[uint64](pvalue.vptr) - by.i32.uint64)
                of vUInt32: cast[pointer](cast[uint64](pvalue.vptr) - by.u32.uint64)
                of vInt64: cast[pointer](cast[uint64](pvalue.vptr) - by.i64.uint64)
                of vUInt64: cast[pointer](cast[uint64](pvalue.vptr) - by.u64.uint64)
                else: nil # impossible
            
            state.stack.add Value(t: pvalue.t, kind: vVoidPtr, vptr: newPtr)
        else:
            raise newException(ValueError, "Expected a pointer")

    of ptr_addr:
        let pvalue = state.stack.pop

        case pvalue.kind
        of vPtr: state.stack.add state.makeUInt64(cast[uint64](pvalue.`ptr`) + pvalue.offset) # not entirely accurate but eh
        of vVoidPtr: state.stack.add state.makeUInt64(cast[uint64](pvalue.vptr))
        else: raise newException(ValueError, "Expected a pointer")
    
    of ptr_resized:
        let newSize = state.stack.pop; assert likely(newSize.kind in {vInt8..vUInt64}), "Expected an integer"
        let pvalue = state.stack.pop; assert likely(pvalue.kind == vPtr), "Expected a typed pointer"

        let size =
            case newSize.kind
            of vInt8: newSize.i8.uint
            of vUInt8: newSize.u8.uint
            of vInt16: newSize.i16.uint
            of vUInt16: newSize.u16.uint
            of vInt32: newSize.i32.uint
            of vUInt32: newSize.u32.uint
            of vInt64: newSize.i64.uint
            of vUInt64: newSize.u64.uint
            else: 0 # impossible
        
        var pbuf: ref seq[Value] not nil
        new(pbuf)
        if pvalue.offset == 0:
            pbuf[] = pvalue.`ptr`[]
            setLen(pbuf[], size - pvalue.offset)
        else:
            pbuf[] = pvalue.`ptr`[pvalue.offset..min(pvalue.offset - size, pvalue.offset - pvalue.`ptr`[].len.uint)]
            setLen(pbuf[], size - pvalue.offset) # TODO: optimize this when size < current len
        
        state.stack.add Value(t: pvalue.t, kind: vPtr, offset: 0, `ptr`: pbuf)

    of ptr_copy_to:
        let pvalue2 = state.stack.pop
        let sizeValue = state.stack.pop; assert sizeValue.kind in {vInt8..vUInt64}, "Expected an integer"
        let pvalue1 = state.stack.pop

        assert pvalue1.kind != pvalue2.kind

        let size =
            case sizeValue.kind
            of vInt8: sizeValue.i8.uint
            of vUInt8: sizeValue.u8.uint
            of vInt16: sizeValue.i16.uint
            of vUInt16: sizeValue.u16.uint
            of vInt32: sizeValue.i32.uint
            of vUInt32: sizeValue.u32.uint
            of vInt64: sizeValue.i64.uint
            of vUInt64: sizeValue.u64.uint
            else: 0 # impossible

        case pvalue1.kind
        of vPtr:
            # TODO: bounds check, null check
            let offset1 = pvalue1.offset
            let offset2 = pvalue2.offset
            pvalue2.`ptr`[offset2..<(offset2 + size)] = pvalue1.`ptr`[offset1..<(offset1 + size)]
        
        of vVoidPtr:
            pvalue1.vptr.copyMem(pvalue2.vptr, size)
        
        else:
            raise newException(ValueError, "Expected a pointer")
    
    of ptr_move_to:
        let pvalue2 = state.stack.pop
        let sizeValue = state.stack.pop; assert sizeValue.kind in {vInt8..vUInt64}, "Expected an integer"
        let pvalue1 = state.stack.pop

        assert pvalue1.kind != pvalue2.kind

        let size =
            case sizeValue.kind
            of vInt8: sizeValue.i8.uint
            of vUInt8: sizeValue.u8.uint
            of vInt16: sizeValue.i16.uint
            of vUInt16: sizeValue.u16.uint
            of vInt32: sizeValue.i32.uint
            of vUInt32: sizeValue.u32.uint
            of vInt64: sizeValue.i64.uint
            of vUInt64: sizeValue.u64.uint
            else: 0 # impossible

        case pvalue1.kind
        of vPtr:
            # TODO: bounds check, null check, actually make it move the data
            let offset1 = pvalue1.offset
            let offset2 = pvalue2.offset
            pvalue2.`ptr`[][offset2..<(offset2 + size)] = pvalue1.`ptr`[][offset1..<(offset1 + size)]
        
        of vVoidPtr:
            pvalue1.vptr.moveMem(pvalue2.vptr, size)
        
        else:
            raise newException(ValueError, "Expected a pointer")
    
    of ptr_cmp_with:
        let pvalue2 = state.stack.pop
        let sizeValue = state.stack.pop; assert sizeValue.kind in {vInt8..vUInt64}, "Expected an integer"
        let pvalue1 = state.stack.pop

        assert pvalue1.kind != pvalue2.kind

        let size =
            case sizeValue.kind
            of vInt8: sizeValue.i8.uint
            of vUInt8: sizeValue.u8.uint
            of vInt16: sizeValue.i16.uint
            of vUInt16: sizeValue.u16.uint
            of vInt32: sizeValue.i32.uint
            of vUInt32: sizeValue.u32.uint
            of vInt64: sizeValue.i64.uint
            of vUInt64: sizeValue.u64.uint
            else: 0 # impossible

        case pvalue1.kind
        of vPtr:
            # TODO: bounds check, elem type check
            let ref1 = pvalue1.`ptr`
            let ref2 = pvalue2.`ptr`
            if ref1 == nil or ref2 == nil:
                state.stack.add state.makeInt32(cmpMem(cast[pointer](ref1), cast[pointer](ref2), size).int32)
                return nil

            let ptr1 = ref1[]
            let ptr2 = ref2[]
            let offset1 = pvalue1.offset
            let offset2 = pvalue2.offset
            #pvalue2.`ptr`[][offset2..(offset2 + size)] = pvalue1.`ptr`[][offset1..(offset1 + size)]
            for i in 0..<size:
                let OFFSET = offsetOf(Value, b).uint64
                let addr1 = cast[uint64](addr ptr1[offset1 + i])
                let addr2 = cast[uint64](addr ptr2[offset2 + i])
                # TODO: maybe make this smarter so it uses different size depending on the tag (or add this logic to the ptr value)
                let c = cmpMem(cast[pointer](addr1 + OFFSET), cast[pointer](addr2 + OFFSET), sizeof(Value).uint64 - OFFSET)
                if c != 0:
                    state.stack.add state.makeInt32(c.int32)
                    return nil
            
            state.stack.add state.makeInt32(0)
        
        of vVoidPtr:
            state.stack.add state.makeInt32(pvalue1.vptr.cmpMem(pvalue2.vptr, size).int32)
        
        else:
            raise newException(ValueError, "Expected a pointer")
    
    of ptr_fill_with:
        let value = state.stack.pop
        let sizeValue = state.stack.pop; assert sizeValue.kind in {vInt8..vUInt64}, "Expected an integer"
        let pvalue = state.stack.pop

        let size =
            case sizeValue.kind
            of vInt8: sizeValue.i8.uint
            of vUInt8: sizeValue.u8.uint
            of vInt16: sizeValue.i16.uint
            of vUInt16: sizeValue.u16.uint
            of vInt32: sizeValue.i32.uint
            of vUInt32: sizeValue.u32.uint
            of vInt64: sizeValue.i64.uint
            of vUInt64: sizeValue.u64.uint
            else: 0 # impossible

        case pvalue.kind
        of vPtr:
            # TODO: bounds check, null check
            let offset = pvalue.offset
            let data = pvalue.`ptr`
            for i in offset..<(offset + size):
                data[][i] = value
        
        of vVoidPtr:
            raiseAssert "NYI!"
        
        else:
            raise newException(ValueError, "Expected a pointer")
    

    # case id
    of caseid_eq: binaryOp(vUInt16, vBool, value1.u16 == value2.u16) # lazy for now
    of caseid_ne: binaryOp(vUInt16, vBool, value1.u16 != value2.u16) # lazy for now
    of caseid_gt: binaryOp(vUInt16, vBool, value1.u16 > value2.u16) # lazy for now
    of caseid_ge: binaryOp(vUInt16, vBool, value1.u16 >= value2.u16) # lazy for now
    of caseid_lt: binaryOp(vUInt16, vBool, value1.u16 < value2.u16) # lazy for now
    of caseid_le: binaryOp(vUInt16, vBool, value1.u16 <= value2.u16) # lazy for now

    # debug
    of debug_print:
        let value = state.stack.pop

        echo state.stringy(value)
    
    #else: raiseAssert "NYI!"

    return nil