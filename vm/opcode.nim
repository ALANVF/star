import util
import ids
import typeref

{.experimental: "notNil".}
{.experimental: "codeReordering".}

public:
 type
    NativeOp {.pure.} = enum
        # all values
        value_eq,
        value_new,
        cast_value_str,
        value_address,
        
        # multi-kinds
        multikind_truthy,
        multikind_has,
        multikind_include,
        multikind_exclude,

        # bools
        bool_not,
        bool_and,
        bool_or,
        bool_xor,
        bool_eq,
        bool_ne,
        cast_bool_i8,
        cast_bool_u8,
        cast_bool_i16,
        cast_bool_u16,
        cast_bool_i32,
        cast_bool_u32,
        cast_bool_i64,
        cast_bool_u64,

        # uint8
        u8_truthy,
        u8_compl,
        u8_succ,
        u8_pred,
        u8_add,
        u8_sub,
        u8_mult,
        u8_pow,
        u8_div,
        u8_idiv,
        u8_mod,
        u8_mod0,
        u8_and,
        u8_or,
        u8_xor,
        u8_shl,
        u8_shr,
        u8_eq,
        u8_ne,
        u8_gt,
        u8_ge,
        u8_lt,
        u8_le,
        cast_u8_bool,
        cast_u8_i8,
        cast_u8_i16,
        cast_u8_u16,
        cast_u8_i32,
        cast_u8_u32,
        cast_u8_i64,
        cast_u8_u64,
        cast_u8_d32,
        cast_u8_d64,
        cast_u8_str,

        # int8
        i8_truthy,
        i8_abs,
        i8_neg,
        i8_compl,
        i8_succ,
        i8_pred,
        i8_add,
        i8_sub,
        i8_mult,
        i8_pow,
        i8_div,
        i8_idiv,
        i8_mod,
        i8_mod0,
        i8_and,
        i8_or,
        i8_xor,
        i8_shl,
        i8_shr,
        i8_eq,
        i8_ne,
        i8_gt,
        i8_ge,
        i8_lt,
        i8_le,
        cast_i8_bool,
        cast_i8_u8,
        cast_i8_i16,
        cast_i8_u16,
        cast_i8_i32,
        cast_i8_u32,
        cast_i8_i64,
        cast_i8_u64,
        cast_i8_d32,
        cast_i8_d64,
        cast_i8_str,

        # uint16
        u16_truthy,
        u16_compl,
        u16_succ,
        u16_pred,
        u16_add,
        u16_sub,
        u16_mult,
        u16_pow,
        u16_div,
        u16_idiv,
        u16_mod,
        u16_mod0,
        u16_and,
        u16_or,
        u16_xor,
        u16_shl,
        u16_shr,
        u16_eq,
        u16_ne,
        u16_gt,
        u16_ge,
        u16_lt,
        u16_le,
        cast_u16_bool,
        cast_u16_i8,
        cast_u16_u8,
        cast_u16_i16,
        cast_u16_i32,
        cast_u16_u32,
        cast_u16_i64,
        cast_u16_u64,
        cast_u16_d32,
        cast_u16_d64,
        cast_u16_str,

        # int16
        i16_truthy,
        i16_abs,
        i16_neg,
        i16_compl,
        i16_succ,
        i16_pred,
        i16_add,
        i16_sub,
        i16_mult,
        i16_pow,
        i16_div,
        i16_idiv,
        i16_mod,
        i16_mod0,
        i16_and,
        i16_or,
        i16_xor,
        i16_shl,
        i16_shr,
        i16_eq,
        i16_ne,
        i16_gt,
        i16_ge,
        i16_lt,
        i16_le,
        cast_i16_bool,
        cast_i16_i8,
        cast_i16_u8,
        cast_i16_u16,
        cast_i16_i32,
        cast_i16_u32,
        cast_i16_i64,
        cast_i16_u64,
        cast_i16_d32,
        cast_i16_d64,
        cast_i16_str,

        # uint32
        u32_truthy,
        u32_compl,
        u32_succ,
        u32_pred,
        u32_add,
        u32_sub,
        u32_mult,
        u32_pow,
        u32_div,
        u32_idiv,
        u32_mod,
        u32_mod0,
        u32_and,
        u32_or,
        u32_xor,
        u32_shl,
        u32_shr,
        u32_eq,
        u32_ne,
        u32_gt,
        u32_ge,
        u32_lt,
        u32_le,
        cast_u32_bool,
        cast_u32_i8,
        cast_u32_u8,
        cast_u32_i16,
        cast_u32_u16,
        cast_u32_i32,
        cast_u32_i64,
        cast_u32_u64,
        cast_u32_d32,
        cast_u32_d64,
        cast_u32_str,

        # int32
        i32_truthy,
        i32_abs,
        i32_neg,
        i32_compl,
        i32_succ,
        i32_pred,
        i32_add,
        i32_sub,
        i32_mult,
        i32_pow,
        i32_div,
        i32_idiv,
        i32_mod,
        i32_mod0,
        i32_and,
        i32_or,
        i32_xor,
        i32_shl,
        i32_shr,
        i32_eq,
        i32_ne,
        i32_gt,
        i32_ge,
        i32_lt,
        i32_le,
        cast_i32_bool,
        cast_i32_i8,
        cast_i32_u8,
        cast_i32_i16,
        cast_i32_u16,
        cast_i32_u32,
        cast_i32_i64,
        cast_i32_u64,
        cast_i32_d32,
        cast_i32_d64,
        cast_i32_str,

        # uint64
        u64_truthy,
        u64_compl,
        u64_succ,
        u64_pred,
        u64_add,
        u64_sub,
        u64_mult,
        u64_pow,
        u64_div,
        u64_idiv,
        u64_mod,
        u64_mod0,
        u64_and,
        u64_or,
        u64_xor,
        u64_shl,
        u64_shr,
        u64_eq,
        u64_ne,
        u64_gt,
        u64_ge,
        u64_lt,
        u64_le,
        cast_u64_bool,
        cast_u64_i8,
        cast_u64_u8,
        cast_u64_i16,
        cast_u64_u16,
        cast_u64_i32,
        cast_u64_u32,
        cast_u64_i64,
        cast_u64_d32,
        cast_u64_d64,
        cast_u64_str,
        #cast_u64_ptr,

        # int64
        i64_truthy,
        i64_abs,
        i64_neg,
        i64_compl,
        i64_succ,
        i64_pred,
        i64_add,
        i64_sub,
        i64_mult,
        i64_pow,
        i64_div,
        i64_idiv,
        i64_mod,
        i64_mod0,
        i64_and,
        i64_or,
        i64_xor,
        i64_shl,
        i64_shr,
        i64_eq,
        i64_ne,
        i64_gt,
        i64_ge,
        i64_lt,
        i64_le,
        cast_i64_bool,
        cast_i64_i8,
        cast_i64_u8,
        cast_i64_i16,
        cast_i64_u16,
        cast_i64_i32,
        cast_i64_u32,
        cast_i64_u64,
        cast_i64_d32,
        cast_i64_d64,
        cast_i64_str,

        # dec32
        d32_nan,
        d32_inf,
        d32_neg_inf,
        d32_pi,
        d32_abs,
        d32_sqrt,
        d32_exp,
        d32_sin,
        d32_cos,
        d32_tan,
        d32_asin,
        d32_acos,
        d32_atan,
        d32_floor,
        d32_ceil,
        d32_trunc,
        d32_round,
        d32_ln,
        d32_log,
        d32_truthy,
        d32_neg,
        d32_succ,
        d32_pred,
        d32_add,
        d32_sub,
        d32_mult,
        d32_pow,
        d32_div,
        d32_idiv,
        d32_mod,
        d32_mod0,
        d32_eq,
        d32_ne,
        d32_gt,
        d32_ge,
        d32_lt,
        d32_le,
        cast_d32_i8,
        cast_d32_u8,
        cast_d32_i16,
        cast_d32_u16,
        cast_d32_i32,
        cast_d32_u32,
        cast_d32_i64,
        cast_d32_u64,
        cast_d32_d64,
        cast_d32_str,

        # dec64
        d64_nan,
        d64_inf,
        d64_neg_inf,
        d64_pi,
        d64_abs,
        d64_sqrt,
        d64_exp,
        d64_sin,
        d64_cos,
        d64_tan,
        d64_asin,
        d64_acos,
        d64_atan,
        d64_floor,
        d64_ceil,
        d64_trunc,
        d64_round,
        d64_ln,
        d64_log,
        d64_truthy,
        d64_neg,
        d64_succ,
        d64_pred,
        d64_add,
        d64_sub,
        d64_mult,
        d64_pow,
        d64_div,
        d64_idiv,
        d64_mod,
        d64_mod0,
        d64_eq,
        d64_ne,
        d64_gt,
        d64_ge,
        d64_lt,
        d64_le,
        cast_d64_i8,
        cast_d64_u8,
        cast_d64_i16,
        cast_d64_u16,
        cast_d64_i32,
        cast_d64_u32,
        cast_d64_i64,
        cast_d64_u64,
        cast_d64_d32,
        cast_d64_str,

        # ptr
        #ptr_new,
        ptr_get_deref,
        ptr_set_deref,
        ptr_get_at,
        ptr_set_at,
        ptr_add,
        ptr_sub,
        ptr_addr,
        ptr_resized,
        ptr_copy_to,
        ptr_move_to,
        ptr_cmp_with,
        ptr_fill_with,

        # case id
        caseid_eq,
        caseid_ne,
        caseid_gt,
        caseid_ge,
        caseid_lt,
        caseid_le


public:
 type
    KOpcode = enum
        # Storage / Access
        oNewLocal, oGetLocal, oSetLocal, oTeeLocal,
        oGetField, oSetField, oTeeField,
        oGetStaticField, oSetStaticField, oTeeStaticField,

        # Stack manip
        oDup, oDup2, oSwap, oPop,

        # Control flow
        oIf, oIfNot, oIfElse,
        oDo,
        oLoop,
        oTry,    
        oRet, oRetVoid,
        oThrow, oRethrow,
        oBreak,
        oNext,

        # Natives
        oNative,

        # Values
        oInt8, oUInt8, oInt16, oUInt16, oInt32, oUInt32, oInt64, oUInt64,
        oDec32, oDec64,
        oChar,
        oStr,
        oTrue, oFalse,
        oThis,
        #oFunc,

        # Comprehension
        oBlock,

        # Operations
        oVCaseID, oTCaseID,
        oKindID, oKindSlot, oKindValue,
        oUpcast, oDowncast, oNativeCast,
        oOfType,
        oNewPtr, oPtrFromAddr,

        # Members
        oGetMember, oSetMember, oTeeMember,
        oGetStaticMember, oSetStaticMember, oTeeStaticMember,

        # Messaging
        oDefaultInit,
        oInitThis_s, oInitThis_m,
        oSend_is,
        oSend_im,
        oSend_ss,
        oSend_ms,
        oSend_si, oSendDyn_si,
        oSend_mi, oSendDyn_mi,
        oSend_c, oSendDyn_c,
        oSend_bo, oSendDyn_bo,
        oSend_uo, oSendDyn_uo,

        # Creation
        oInitClass,
        oInitTKind, oInitVKind,
        oInitMultiTKind, oInitMultiVKind,

        # Multi kinds
        oMultiKindHasTag, oMultiKindGetTag,
        oMultiKindGetSlot
    
    Opcodes = ref seq[Opcode] not nil # forced to be ref type :(
    
    Opcode {.shallow.} = object
        case kind: KOpcode
        of oNewLocal: nil
        of oGetLocal..oTeeLocal:
            localID: LocalID
        of oGetField..oTeeStaticField:
            fieldID: FieldID

        of oDup..oPop: nil
        
        of oIf, oIfNot:
            ifThen: Opcodes
        of oIfElse:
            then, `else`: Opcodes
        of oDo:
            do_id: LoopID
            do_body: Opcodes
        of oLoop:
            loop_id: LoopID
            loop_body: Opcodes
            loop_then: nil Opcodes
        of oTry:
            `try`, catch: Opcodes
        of oRet, oRetVoid: nil
        of oThrow:
            throw: string
        of oRethrow: nil
        of oBreak, oNext:
            labelID: LoopID
        
        of oNative:
            native: NativeOp

        of oInt8: i8: int8
        of oUInt8: u8: uint8
        of oInt16: i16: int16
        of oUInt16: u16: uint16
        of oInt32: i32: int32
        of oUInt32: u32: uint32
        of oInt64: i64: int64
        of oUInt64: u64: uint64
        of oDec32: d32: float32
        of oDec64: d64: float64
        of oChar: c: uint8
        of oStr: s: string
        of oTrue, oFalse, oThis: nil
        #of oFunc: nil

        of oBlock: `block`: Opcodes

        of oVCaseID, oTCaseID:
            cid_t: TypeRef
            cid_tag: KindTag
        of oKindID: nil
        of oKindSlot: slot: uint8
        of oKindValue: nil
        of oUpcast..oNativeCast: target: TypeRef
        of oOfType: of_t: TypeRef
        of oNewPtr, oPtrFromAddr: ptr_t: TypeRef
        
        of oGetMember..oTeeMember:
            instID: MemberID
        of oGetStaticMember..oTeeStaticMember:
            owner: TypeRef
            staticID: MemberID

        of oDefaultInit:
            di_t: TypeRef
        of oInitThis_s, oInitThis_m:
            super: TypeRef
            init_id: InitID
            init_ctx: nil TypeVarInstCtx
        of oSend_is:
            is_t: TypeRef
            is_id: InitID
        of oSend_im:
            im_t: TypeRef
            im_id: InitID
            im_ctx: nil TypeVarInstCtx
        of oSend_ss:
            ss_t: TypeRef
            ss_id: MethodID
        of oSend_ms:
            ms_t: TypeRef
            ms_id: MethodID
            ms_ctx: nil TypeVarInstCtx
        of oSend_si, oSendDyn_si:
            si_t: TypeRef
            si_id: MethodID
        of oSend_mi, oSendDyn_mi:
            mi_t: TypeRef
            mi_id: MethodID
            mi_ctx: nil TypeVarInstCtx
        of oSend_c, oSendDyn_c:
            cm_t: TypeRef
            cm_id: MethodID
            cm_ctx: nil TypeVarInstCtx
        of oSend_bo, oSendDyn_bo:
            bo_t: TypeRef
            bo_id: MethodID
            bo_ctx: nil TypeVarInstCtx
        of oSend_uo, oSendDyn_uo:
            uo_t: TypeRef
            uo_id: MethodID

        of oInitClass:
            class_t: TypeRef
        of oInitTKind..oInitMultiVKind:
            kind_t: TypeRef
            kind_tag: KindTag

        of oMultiKindHasTag, oMultiKindGetTag:
            mk_tag: KindTag
        of oMultiKindGetSlot:
            mks_tag: KindTag
            mks_slot: uint8