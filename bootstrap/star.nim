import protocol
import macros
import strutils
import math
import system/[memory, memalloc]

{.experimental.}

type
    StarValue* = ref object of RootObj

#[type
    StarKind[T: Ordinal]* = ref object of StarValue

type
    StarMultiKind[T: Ordinal; Ts: set = set[T]]* = ref object of StarValue]#


proc `*value-eq`*[This: StarValue](this, other: This): bool =
    return this == other

proc `*value-truthy`*[This: StarValue](this: This): bool =
    return true

proc `*cast-value-str`*[This: StarValue](this: This): string =
    result = $This & '['
    
    var fields: seq[string] = @[]
    
    for f, v in fieldPairs(this):
        fields.add f & ": " & $v
    
    if fields.len == 0:
        result.add "new]"
    else:
        result.add fields.join " "
        result.add ']'


# NUM NATIVES

proc `*num-truthy`*[This: SomeNumber](this: This): bool {.inline.} =
    return this != 0.This


# INT NATIVES

proc `*int-pow`*[This, T: SomeInteger](this: This, other: T): This {.inline.} =
    when U is SomeSignedInt:
        if other < 0.T:
            return 0.T # or maybe throw an error?
        else:
            return this ^ other.Natural
    else:
        return this ^ other

proc `*int-pow`*[This: SomeInteger, T: SomeFloat](this: This, other: T): T {.inline.} =
    return pow(this.T, other)

proc `*int-mod0`*[This: SomeInteger](this, other: This): bool {.inline.} =
    return this % other == 0.This


# DEC NATIVES

proc `*dec-pi`*[This: SomeFloat](): This {.inline.} =
    return PI.This

proc `*dec-pow`*[This: SomeFloat](this, other: This): This {.inline.} =
    return pow(this, other)

proc `*dec-mod0`*[This: SomeFloat](this, other: This): bool {.inline.} =
    return this mod other == 0.This


# PTR NATIVES

proc `*ptr-new`*[T](size: int): ptr T {.inline.} =
    if size == 0:
        return create(T)
    else:
        return create(T, size.Positive)

proc `*ptr-at`*[T](this: ptr T, offset: int): T {.inline.} =
    return cast[ptr UncheckedArray[T]](this)[offset]

proc `*ptr-set-at`*[T](this: ptr T, offset: int, value: T) {.inline.} =
    cast[ptr UncheckedArray[T]](this)[offset] = value

proc `*ptr-add`*[T](this: ptr T, offset: int): ptr T {.inline.} =
    return cast[ptr T](cast[int](this) + sizeof(T)*offset)

proc `*ptr-sub`*[T](this: ptr T, offset: int): ptr T {.inline.} =
    return cast[ptr T](cast[int](this) - sizeof(T)*offset)

proc `*ptr-resized`*[T](this: ptr T, newSize: int): ptr T {.inline.} =
    return resize(this, newSize.Natural)

proc `*ptr-copy-to`*[T](this: ptr T, length: int, dest: ptr T) {.inline.} =
    copyMem(cast[pointer](dest), cast[pointer](this), length.Natural)

proc `*ptr-move-to`*[T](this: ptr T, length: int, dest: ptr T) {.inline.} =
    moveMem(cast[pointer](dest), cast[pointer](this), length.Natural)

proc `*ptr-cmp-with`*[T](this: ptr T, length: int, other: ptr T): int32 {.inline.} =
    return nimCmpMem(cast[pointer](this), cast[pointer](other), length.Natural)

proc `*ptr-fill-with`*[T](this: ptr T, length: int, value: T) {.inline.} =
    let t = cast[ptr UncheckedArray[T]](this)
    for i in 0..<size:
        t[i] = v


# OTHER STUFF

macro `*all`*(head: untyped, conds: varargs[untyped]): untyped =
    return quote "@":
        if @head:
            @(
                if conds.len == 0:
                    quote: true
                else:
                    quote "@": `*all`(@conds)
            )
        else:
            false

macro `*any`*(head: untyped, conds: varargs[untyped]): untyped =
    return quote "@":
        if @head:
            true
        else:
            @(
                if conds.len == 0:
                    quote: false
                else:
                    quote "@": `*any`(@conds)
            )

macro `*one`*(head1, head2: untyped, conds: varargs[untyped]): untyped =
    if conds.len == 0:
        return quote:
            (`head1`) xor (`head2`)
    else:
        let
            a = genSym()
            b = genSym()
            recCall = quote "@":
                `*one`(@a xor @b, @conds)
        
        return quote:
            block:
                let
                    `a` = `head1`
                    `b` = `head2`
                if `a` and `b`:
                    false
                else:
                    `recCall`

macro `*none`*(head: untyped, conds: varargs[untyped]): untyped =
    return quote "@":
        if @head:
            false
        else:
            @(
                if conds.len == 0:
                    quote: true
                else:
                    quote "@": `*none`(@conds)
            )