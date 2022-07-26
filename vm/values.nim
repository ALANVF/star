import util
import dec64
import ids
import typeref
import std/tables

{.experimental: "notNil".}

public:
 type
    KValue = enum
        vVoid, # Acts like a unit value
        vBool,
        vInt8, vUInt8, vInt16, vUInt16, vInt32, vUInt32, vInt64, vUInt64,
        vFloat32, vFloat64, vDec64,
        vPtr, vVoidPtr,

        vClass,
        vVKind,
        vTKind, vTKindClass,
        vVMultiKind, vTMultiKind
    
    VMultiKindMap = Table[KindTag, ref Value]
    TMultiKindMap = Table[KindTag, ref seq[Value]]
    
    Value = object
        t: TypeRef
        case kind: KValue
        of vVoid: nil
        of vBool: b: bool
        of vInt8: i8: int8
        of vUInt8: u8: uint8
        of vInt16: i16: int16
        of vUInt16: u16: uint16
        of vInt32: i32: int32
        of vUInt32: u32: uint32
        of vInt64: i64: int64
        of vUInt64: u64: uint64
        of vFloat32: f32: float32
        of vFloat64: f64: float64
        of vDec64: d64: Dec64
        of vPtr:
            offset: uint
            `ptr`: ref seq[Value]
        of vVoidPtr: vptr: pointer

        of vClass:
            c_fields: ref seq[Value] not nil
        of vVKind:
            vk_id: KindTag
            vk_value: ref Value
        of vTKind:
            tk_id: KindTag
            tk_slots: ref seq[Value]
        of vTKindClass:
            tkc_id: KindTag
            tkc_slots: ref seq[Value]
            tkc_fields: ref seq[Value] not nil
        of vVMultiKind:
            vmk_map: VMultiKindMap
        of vTMultiKind:
            tmk_map: TMultiKindMap