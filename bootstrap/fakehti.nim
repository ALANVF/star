# Basically a copy of https://github.com/nim-lang/Nim/blob/devel/lib/system/hti.nim

type
  # This should be the same as ast.TTypeKind
  # many enum fields are not used at runtime
  TNimKind* {.pure.} = enum
    tyNone,
    tyBool,
    tyChar,
    tyEmpty,
    tyArrayConstr,
    tyNil,
    tyUntyped,
    tyTyped,
    tyTypeDesc,
    tyGenericInvocation, # ``T[a, b]`` for types to invoke
    tyGenericBody,       # ``T[a, b, body]`` last parameter is the body
    tyGenericInst,       # ``T[a, b, realInstance]`` instantiated generic type
    tyGenericParam,      # ``a`` in the example
    tyDistinct,          # distinct type
    tyEnum,
    tyOrdinal,
    tyArray,
    tyObject,
    tyTuple,             # WARNING: The compiler uses tyTuple for pure objects!
    tySet,
    tyRange,
    tyPtr,
    tyRef,
    tyVar,
    tySequence,
    tyProc,
    tyPointer,
    tyOpenArray,
    tyString,
    tyCstring,
    tyForward,
    tyInt,
    tyInt8,
    tyInt16,
    tyInt32,
    tyInt64,
    tyFloat,
    tyFloat32,
    tyFloat64,
    tyFloat128,
    tyUInt,
    tyUInt8,
    tyUInt16,
    tyUInt32,
    tyUInt64,
    tyOwned, tyUnused1, tyUnused2,
    tyVarargsHidden,
    tyUncheckedArray,
    tyProxyHidden,
    tyBuiltInTypeClassHidden,
    tyUserTypeClassHidden,
    tyUserTypeClassInstHidden,
    tyCompositeTypeClassHidden,
    tyInferredHidden,
    tyAndHidden, tyOrHidden, tyNotHidden,
    tyAnythingHidden,
    tyStaticHidden,
    tyFromExprHidden,
    tyOptDeprecated,
    tyVoidHidden

  TNimNodeKind* {.pure.} = enum nkNone, nkSlot, nkList, nkCase
  TNimNode* = object
    kind: TNimNodeKind
    offset: int
    typ: ptr TNimType
    name: cstring
    len: int
    sons: ptr array[0x7fff, ptr TNimNode]

  TNimTypeFlag* {.pure.} = enum
    ntfNoRefs = 0,     # type contains no tyRef, tySequence, tyString
    ntfAcyclic = 1,    # type cannot form a cycle
    ntfEnumHole = 2    # enum has holes and thus `$` for them needs the slow
                       # version
  TNimType* = object
    when defined(gcHooks):
      head*: pointer
    size*: int
    align*: int
    kind*: TNimKind
    flags*: set[TNimTypeFlag]
    base*: ptr TNimType
    node*: ptr TNimNode # valid for tyRecord, tyObject, tyTuple, tyEnum
    finalizer: pointer # the finalizer for the type
    marker: proc (p: pointer, op: int) {.nimcall, tags: [], raises: [].} # marker proc for GC
    deepcopy: proc (p: pointer): pointer {.nimcall, tags: [], raises: [].}
    when defined(nimSeqsV2):
      typeInfoV2*: pointer
    when defined(nimTypeNames):
      name*: cstring
      nextType*: ptr TNimType
      instances: int # count the number of instances
      sizes: int # sizes of all instances in bytes
  
  PNimType* = ptr TNimType