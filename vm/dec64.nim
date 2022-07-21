## Taken from https://github.com/vpisarev/DEC64/blob/alt/dec64.c

import bitops
import math
import util

type
    Coef = -36028797018963968 .. 36028797018963967 # 56-bit int
    Exp = int8
    Dec64* {.packed.} = object
        coef {.bitsize: 56.}: Coef
        exp {.bitsize: 8.}: Exp

converter toDec64*(i: SomeInteger): Dec64 {.inline.} =
    return Dec64(coef: i shr 8, exp: cast[int8](i and 255))

converter toUint64*(d: Dec64): uint64 {.inline.} =
    return cast[uint64](d)

converter toInt64*(d: Dec64): int64 {.inline.} =
    return cast[int64](d)

const
    DEC64_ZERO* = Dec64(coef: 0, exp: 0)
    DEC64_NAN* = Dec64(coef: 0x80, exp: -0x80)
    DEC64_ONE* = Dec64(coef: 1, exp: 0)
    DEC64_NEG_ONE* = Dec64(coef: -1, exp: 0)
    DEC64_2* = Dec64(0x200)
    DEC64_E* = Dec64(0x6092A113D8D574F0)
    DEC64_HALF* = Dec64(0x5FF)
    DEC64_HALF_PI* = Dec64(0x37CE4F32BB21A6F0)
    DEC64_NHALF_PI* = Dec64(0xC831B0CD44DE59F0)
    DEC64_NPI* = Dec64(0x9063619A89BCB4F0)
    DEC64_PI* = Dec64(0x6F9C9E6576434CF0)
    DEC64_2PI* = Dec64(0x165286144ADA42F1)

    POWER = [
        1'i64,
        10'i64,
        100'i64,
        1000'i64,
        10000'i64,
        100000'i64,
        1000000'i64,
        10000000'i64,
        100000000'i64,
        1000000000'i64,
        10000000000'i64,
        100000000000'i64,
        1000000000000'i64,
        10000000000000'i64,
        100000000000000'i64,
        1000000000000000'i64,
        10000000000000000'i64,
        100000000000000000'i64,
        1000000000000000000'i64,
        cast[int64](10000000000000000000'u64),
        0'i64
    ]


proc `==`*(x, y: Dec64): bool
proc eps_equal*(x, y, eps: Dec64): bool
proc is_integer*(d: Dec64): bool
proc `<`*(x, y: Dec64): bool
#proc `<=`*(x, y: Dec64): bool
#proc `>`*(x, y: Dec64): bool
#proc `>=`*(x, y: Dec64): bool
proc is_nan*(d: Dec64): bool
proc is_zero*(d: Dec64): bool
proc `+`*(x, y: Dec64): Dec64
proc `-`*(x, y: Dec64): Dec64
proc `*`*(x, y: Dec64): Dec64
proc `/`*(x, y: Dec64): Dec64
#proc `+=`*(x: var Dec64, y: Dec64) {.inline.}
#proc `-=`*(x: var Dec64, y: Dec64) {.inline.}
#proc `*=`*(x: var Dec64, y: Dec64) {.inline.}
#proc `/=`*(x: var Dec64, y: Dec64) {.inline.}
proc fda*(x, y, z: Dec64): Dec64
proc floor*(d: Dec64): Dec64
proc ceil*(d: Dec64): Dec64
proc floor_div*(x, y: Dec64): Dec64
proc `mod`*(x, y: Dec64): Dec64
proc fma*(x, y, z: Dec64): Dec64
proc `-`*(d: Dec64): Dec64
proc abs*(d: Dec64): Dec64
proc normal*(d: Dec64): Dec64
proc round*(d, place: Dec64): Dec64
proc signum*(d: Dec64): Dec64
proc dec64_from_double*(d: float64): Dec64
proc acos*(slope: Dec64): Dec64
proc asin*(slope: Dec64): Dec64
proc atan*(slope: Dec64): Dec64
proc atan2*(y, x: Dec64): Dec64
proc cos*(radians: Dec64): Dec64
proc exp*(d: Dec64): Dec64
proc factorial*(d: Dec64): Dec64
proc log*(d: Dec64): Dec64
proc pow*(coef, exp: Dec64): Dec64
proc random*(_: type Dec64): Dec64
proc root*(_: type Dec64, index, radicand: Dec64): Dec64
proc seed*(_: type Dec64, part0, part1: uint64): void
proc sin*(radians: Dec64): Dec64
proc sqrt*(radicand: Dec64): Dec64
proc tan*(radians: Dec64): Dec64

# -170141183460469231731687303715884105728 .. 170141183460469231731687303715884105727
# 0x8000_0000_0000_0000_0000_0000_0000_0000 .. 0x7FFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF
#type Int128 = -170141183460469231731687303715884105728 .. 170141183460469231731687303715884105727
type
    Int128 {.importc: "__int128".} = object
        a, b: uint64
    UInt128* {.importc: "unsigned __int128".} = object
        a, b: uint64

proc makeDec64*(coef: int64, exp: int): Dec64 =
    var x = coef
    var e = exp
    
    if x == 0 or e <= -148: return DEC64_ZERO
    while e <= 127:
        let signmask = x shr 63 # signmask is -1 if x is negative, or 0 if positive
        let x_abs = uint64(x xor signmask)
        if x_abs >= 3602879701896396800'u64:
            # pack large ...
            x = (x xor signmask) - signmask
            {.emit: [x, " = (", int64, ")((", uint64, ")((unsigned __int128)", x, " * (", uint64, ")-3689348814741910323 >> 64) >> 3);"].}
            x = (x xor signmask) - signmask
            inc e
        else:
            var deficit = (x_abs > 36028797018963967'u64).int + (x_abs > 360287970189639679'u64).int
            deficit = max(-127 - e, deficit)
            if deficit == 0:
                # this is the "hot path":
                #   1. enter makeDec64
                #   2. enter while loop since e <= 127 most of the time
                #   3. pass quite liberal mantissa "smallness" check
                #   4. pass few more checks to ensure that deficit == 0
                #   5. and here we are, just pack the result and go.
                # that is, while makeDec64() looks heavy, most of the time
                # it finishes fast without a single multiplication or division,
                # just a few well predictable branches,
                # simple arithmetic and bit operations.
                if x != 0:
                    return Dec64(coef: x, exp: e.int8)
                else:
                    return DEC64_ZERO
            # pack increase
            if deficit >= 20: return DEC64_ZERO # underflow
            let scale = POWER[deficit]
            if x > 0:
                x = (x + (scale shr 1)) div scale
            else:
                x = (x - (scale shr 1)) div scale
            e += deficit
    
    if e >= 148: return DEC64_NAN

    # If the exponent is too big (greater than 127).
    # We can attempt to reduce it by scaling back.
    # This can salvage values in a small set of cases.
    let abs_x = x.abs
    let lsb =
        if abs_x == 0: 63
        else: countLeadingZeroBits(abs_x) - 1
    var log10_scale =
        if lsb > 0: (lsb * 77) shr 8
        else: 0
    log10_scale = min(e - 127, log10_scale)
    x *= POWER[log10_scale] # in theory, this shouldn't overflow

    while e > 127:
        # try multiplying the coefficient by 10
        # if it overflows, we failed to salvage
        try:
            x *= 10
        except OverflowDefect:
            return DEC64_NAN

        dec e # decrement the exponent and repeat if necessary

    if uint64((x shr 55) + 1) > 1:
        return DEC64_NAN
    
    # check for overflow
    if x != 0:
        return Dec64(coef: x, exp: e.int8)
    else:
        return DEC64_ZERO


proc `==`*(x, y: Dec64): bool =
    let xi = x.int64
    let yi = y.int64

    var ediff = x.exp.int - y.exp.int
    if ediff == 0:
        return x.exp == -128 or xi == yi
    
    if (xi xor yi) < 0: return false

    # Let's do exact comparison instead of relying on dec64.`-`() rounding logic.
    # This is also faster, because we do not need to pack the final result, and
    # we don't have to fix possible overflow; instead we use the fact of
    # overflow to claim inequality
    
    # If user wants to check whether x ~ y,
    # they can use the slower dec64.is_zero(dec64.`-`(x, y))

    # Before comparison can take place, the exponents must be made to match.
    # Swap the numbers if the second exponent is greater than the first.
    let (x1, y1) =
        if ediff < 0: (yi, xi)
        else: (xi, yi)
    ediff = abs ediff

    # if one of the arguments is nan or the difference between
    # exponents is very big, they are not equal
    if ediff > 17 or x.exp == -128 or y.exp == -128: return false

    # try to bring e0 -> e1 by scaling up x.
    # before scaling x's exponent bits are cleared not to
    # affect the coefficient bits.
    # If we get overflow, it means that x cannot be represented
    # with the same exponent as y, which means that x != y
    let x_scaled =
        try:
            (x1 and not 255) * POWER[ediff]
        except OverflowDefect:
            return false
    
    return x_scaled == (y1 and not 255)

proc eps_equal*(x, y, eps: Dec64): bool =
    if x == y:
        return true
    else:
        return abs(x - y) < eps


proc is_integer*(d: Dec64): bool =
    # If the number contains a non-zero fractional part or if it is nan,
    # return false. Otherwise, return true.
    let x = d.coef
    let e = d.exp
    # a positive exponent means an integer,
    # zero is an integer and
    # nan is not an integer
    if e >= 0 or (x == 0 and e != -128):
        return true
    # huge negative exponents can never be int,
    # this check handles nan too.
    if e < -17: return false
    return x mod POWER[-e] == 0


proc `<`*(x, y: Dec64): bool =
    # Compare two dec64 numbers. If the first is less than the second, return true,
    # otherwise return false. Any nan value is greater than any number value

    # If the exponents are the same, then do a simple compare.
    let ex = x.exp
    let ey = y.exp
    if ex == ey:
        return ex != -128 and x.int64 < y.int64

    # The exponents are not the same.
    if ex == -128: return false
    if ey == -128: return true

    var ediff = ex - ey
    let cx = x.coef
    let cy = y.coef
    if ediff > 0:
        # The maximum cofficient is 36028797018963967. 10**18 is more than that.
        ediff = min(ediff, 18)
        # We need to make them conform before we can compare. Multiply the first
        # coefficient by 10**(first exponent - second exponent)
        var x_scaled {.noInit.}: Int128
        {.emit: [x_scaled, " = (__int128)", cx, "*", POWER[ediff], ";"].}
        var x_high {.noInit.}: Dec64
        {.emit: [x_high, "(", Dec64, ")(", x_scaled, " >> 64);"].}
        let x2 = cast[Dec64](x_scaled)
        
        # in the case of overflow check the sign of higher 64-bit half;
        # otherwise compare numbers with equalized exponents
        if x_high.int64 == x2.int64 shr 63:
            return x2.int64 < cy
        else:
            return x_high.int64 < 0
    else:
        # The maximum cofficient is 36028797018963967. 10**18 is more than that.
        ediff = min(-ediff, 18)
        # We need to make them conform before we can compare. Multiply the first
        # coefficient by 10**(first exponent - second exponent)
        var y_scaled {.noInit.}: Int128
        {.emit: [y_scaled, " = (__int128)", cy, "*", POWER[ediff], ";"].}
        var y_high {.noInit.}: Dec64
        {.emit: [y_high, "(", Dec64, ")(", y_scaled, " >> 64);"].}
        let y2 = cast[Dec64](y_scaled)
        
        # in the case of overflow check the sign of higher 64-bit half;
        # otherwise compare numbers with equalized exponents
        if y_high.int64 == y2.int64 shr 63:
            return cx < y2.int64
        else:
            return y_high.int64 >= 0

proc `<=`*(x, y: Dec64): bool {.inline.} =
    return not (y < x)

proc `>`*(x, y: Dec64): bool {.inline.} =
    return y < x

proc `>=`*(x, y: Dec64): bool {.inline.} =
    return not (x < y)


proc is_nan*(d: Dec64): bool =
    return d.exp == -128


proc is_zero*(d: Dec64): bool =
    if d.exp == -128: return false
    return (d.int64 and not 255) == 0


proc add_slow(cx: int64, ex: int, cy: int64, ey: int): Dec64 =
    # The slower path is taken when the exponents are different.
    # Before addition can take place, the exponents must be made to match.
    # Swap the numbers if the second exponent is greater than the first.
    var (r0, r1, e0, e1) =
        if ex > ey: (cx, cy, ex, ey)
        else: (cy, cx, ey, ex)
    var r0_0 = r0
    var e0_0 = e0

    # it's enough to check only e1 or -128;
    # if e1 is not 128, e0 cannot be -128, since it's greater
    if e1 == -128: return DEC64_NAN
    if r0 == 0: e0 = e1

    if e0 > e1:
        let abs_r0 = abs r0
        let lsb =
            if abs_r0 == 0: 63
            else: countLeadingZeroBits(abs_r0) - 1
        var log10_scale =
            if lsb > 0: (lsb * 77) shr 8
            else: 0
        log10_scale = min(e0 - e1, log10_scale)
        r0 *= POWER[log10_scale] # in theory, this should not overflow
        e0 -= log10_scale

        while e0 > e1:
            # First, try to decrease the first exponent using "lossless" multiplication
            # of the first coefficient by multiplying it by 10 at a time.
            try:
                r0 *= 10
            except OverflowDefect:
                # We cannot decrease the first exponent any more, so we must instead try to
                # increase the second exponent, which will result in a loss of significance.
                # That is the heartbreak of floating point.

                # Determine how many places need to be shifted. If it is more than 17, there is
                # nothing more to add.
                let ediff = e0 - e1
                if ediff > 10: return makeDec64(r0_0, e0_0)
                r1 = r1 div POWER[ediff]
                if r1 == 0: return makeDec64(r0_0, e0_0)
                return makeDec64(r0 + r1, e0)
            
            dec e0
    
    return makeDec64(r0 + r1, e0)

proc `+`*(x, y: Dec64): Dec64 =
    # Add two dec64 numbers together.
    # If the two exponents are both zero (which is usually the case for integers)
    # we can take the fast path. Since the exponents are both zero, we can simply
    # add the numbers together and check for overflow.

    if (x.exp or y.exp) == 0:
        # integer case: both exponents are zero.
        try:
            return Dec64(x.int64 + y.int64)
        except OverflowDefect:
            # If there was an overflow (extremely unlikely) then we must make it fit.
            # pack knows how to do that.
            return makeDec64(x.coef + y.coef, 0)
    elif (x.exp xor y.exp) == 0:
        let e = x.exp
        if e == -128: return DEC64_NAN
        
        # The exponents match so we may add now. Zero out one of the exponents so there
        # will be no carry into the coefficients when the coefficients are added.
        # If the result is zero, then return the normal zero.
        try:
            let r = x.coef + y.coef
            if r == 0:
                return DEC64_ZERO
            else:
                return Dec64(coef: r, exp: e)
        except OverflowDefect:
            return makeDec64(x.coef + y.coef, e)
    else:
        return add_slow(x.coef, x.exp, y.coef, y.exp)


proc `-`*(x, y: Dec64): Dec64 =
    # Add two dec64 numbers together.
    # If the two exponents are both zero (which is usually the case for integers)
    # we can take the fast path. Since the exponents are both zero, we can simply
    # add the numbers together and check for overflow.

    if (x.exp or y.exp) == 0:
        # integer case: both exponents are zero.
        try:
            return Dec64(x.int64 - y.int64)
        except OverflowDefect:
            # If there was an overflow (extremely unlikely) then we must make it fit.
            # pack knows how to do that.
            return makeDec64(x.coef - y.coef, 0)
    elif (x.exp xor y.exp) == 0:
        let e = x.exp
        if e == -128: return DEC64_NAN
        
        # The exponents match so we may add now. Zero out one of the exponents so there
        # will be no carry into the coefficients when the coefficients are added.
        # If the result is zero, then return the normal zero.
        try:
            let r = x.coef - y.coef
            if r == 0:
                return DEC64_ZERO
            else:
                return Dec64(coef: r, exp: e)
        except OverflowDefect:
            return makeDec64(x.coef - y.coef, e)
    else:
        return add_slow(x.coef, x.exp, -y.coef, y.exp)


proc `*`*(x, y: Dec64): Dec64 =
    # Multiply two dec64 numbers together.
    let ex = x.exp.int
    let ey = y.exp.int

    let cx = x.coef
    let cy = y.coef

    # The result is nan if one or both of the operands is nan and neither of the
    # operands is zero.
    if (cx == 0 and ex != -128) or (cy == 0 and ey != -128): return DEC64_ZERO
    if ex == -128 and ey == -128: return DEC64_NAN

    var r_big {.noInit.}: Int128
    {.emit: [r_big, " = (__int128)", cx, "*", cy, ";"].}
    var r_high {.noInit.}: int64
    {.emit: [r_high, " = (", int64, ")(", r_big, " >> 64);"].}
    var r = cast[int64](r_big)
    let e = ex.int + ey.int
    if r_high == r shr 63: # no overflow
        return makeDec64(r, e)
    
    let abs_r_high = Dec64(abs r_high.int64)
    let delta_er =
        if abs_r_high.int64 == 0: 1
        else: (((63 - countLeadingZeroBits(abs_r_high.int64)) * 77) shr 8) + 2
    
    # divide by the power of ten & pack the final result
    {.emit: [r, " = (", r_big, " / ", POWER[delta_er], ");"].}
    return makeDec64(r, e + delta_er)


const
    FAST_TAB = [
        (1u8, 0u8), (5u8, 1u8), (0u8, 0u8), (25u8, 2u8), (2u8, 1u8), (0u8, 0u8), (0u8, 0u8), (125u8, 3u8), (0u8, 0u8), (1u8, 1u8),
        (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (5u8, 2u8),
        (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (4u8, 2u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8),
        (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (25u8, 3u8),
        (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (0u8, 0u8), (2u8, 2u8),
    ]
proc divide(x, y: Dec64): tuple[status: -1..1, q: int64, qexp: int] =
    type Status = type(result.status)

    # (x: Dec64, y: Dec64) returns quotient: Dec64
    # Divide a dec64 number by another.
    # Begin unpacking the components.
    var ex = x.exp.int
    var ey = y.exp.int
    var cx = x.coef.int64
    var cy = y.coef.int64
    if x == 0 and ex != -128:
        return (status: Status 0, q: 0'i64, qexp: 0)
    if ex == -128 or ey == -128 or cy == 0:
        return (status: Status -1, q: 0'i64, qexp: -128)

    # if both x and y are even then we can simplify the ratio lossless
    let b = min(countTrailingZeroBits(cx), countTrailingZeroBits(cy))
    cx = cx shr b
    cy = cy shr b

    let abs_y = abs(cy).uint64
    var scale = 0
    if abs_y <= 50:
        scale = FAST_TAB[abs_y-1][0].int
        if scale != 0:
            # fast division by some popular small constants
            # x/2 ~ (x*5)/10, x/5 ~ (x*2)/10, ...
            # and division by a power of 10 is just shift of the exponent
            return (
                status: Status 1,
                q: cx * (if cy < 0: -scale else: scale),
                qexp: ex - ey - FAST_TAB[abs_y-1][1].int
            )
    
    # We want to get as many bits into the quotient as possible in order to capture
    # enough significance. But if the quotient has more than 64 bits, then there
    # will be a hardware fault. To avoid that, we compare the magnitudes of the
    # dividend coefficient and divisor coefficient, and use that to scale the
    # dividend to give us a good quotient.
    let log2_y = 63 - countLeadingZeroBits(abs_y)
    var log10_prescale = 0

    while true:
        let abs_x = abs(cx).uint64
        let log2_x = 63 - countLeadingZeroBits(abs_x)

        # Scale up the dividend to be approximately 58 bits longer than the divisor.
        # Scaling uses factors of 10, so we must convert from a bit count to a digit
        # count by multiplication by 77/256 (approximately LN2/LN10).
        log10_prescale = ((log2_y + 58 - log2_x) * 77) shr 8
        if log10_prescale <= 18: break

        # If the number of scaling digits is larger than 18, then we will have to
        # scale in two steps: first prescaling the dividend to fill a register, and
        # then repeating to fill a second register. This happens when the divisor
        # coefficient is much larger than the dividend coefficient.
        
        # we want 58 bits or so in the dividend
        log10_prescale = ((58 - log2_x) * 77) shr 8
        cx *= POWER[log10_prescale]
        ex -= log10_prescale
    
    # Multiply the dividend by the scale factor, and divide that 128 bit result by
    # the divisor. Because of the scaling, the quotient is guaranteed to use most
    # of the 64 bits in r0, and never more. Reduce the final exponent by the number
    # of digits scaled.
    {.emit: [result.q, " = (", int64, ")((__int128)", cx * POWER[log10_prescale] div y, ");"].}
    result.qexp = ex - ey - log10_prescale
    result.status = 1
    return

proc `/`*(x, y: Dec64): Dec64 =
    let (status, q, qexp) = divide(x, y)
    if status == 0:
        return DEC64_ZERO
    elif status == -1:
        return DEC64_NAN
    else:
        return makeDec64(q, qexp)


proc `+=`*(x: var Dec64, y: Dec64) {.inline.} =
    x = x + y

proc `-=`*(x: var Dec64, y: Dec64) {.inline.} =
    x = x - y

proc `*=`*(x: var Dec64, y: Dec64) {.inline.} =
    x = x * y

proc `/=`*(x: var Dec64, y: Dec64) {.inline.} =
    x = x / y


proc fda*(x, y, z: Dec64): Dec64 =
    let ez = z.exp
    if ez == -128: return DEC64_NAN

    let (status, q, eq) = divide(x, y)
    if status == 0: return z
    if status == -1: return DEC64_NAN
    return add_slow(q, eq, z.coef, ez)


proc to_int(d: Dec64, round_dir: int): Dec64 =
    # Produce the largest integer that is less than or equal to 'x' (round_dir == -1)
    # or greater than or equal to 'x' (round_dir == 1).
    # In the result, the exponent will be greater than or equal to zero unless it is nan.
    # Numbers with positive exponents will not be modified,
    # even if the numbers are outside of the safe integer range.

    var e = d.exp
    var c = d.coef
    if e == -128: return DEC64_NAN

    e = -e
    var rem: int64
    if e < 17:
        let p = POWER[e]
        let c_scaled = c div p
        rem = c - (c_scaled * p)
        if rem == 0:
            return Dec64(c_scaled shl 8)
        c = c_scaled
    else:
        # deal with a micro number
        rem = c
        c = 0
    let delta = ((rem xor round_dir) >= 0).int * round_dir
    return Dec64((c + delta) shl 8)

proc floor*(d: Dec64): Dec64 =
    return to_int(d, -1)


proc ceil*(d: Dec64): Dec64 =
    return to_int(d, 1)


proc floor_div*(x, y: Dec64): Dec64 =
    let ex = x.exp
    let ey = y.exp
    if ex == ey:
        let cx = x.coef
        let cy = y.coef
        if cx == 0 and ex != -128: return DEC64_ZERO # 0/y ~ 0, even if y == 0 or y == nan
        if ex == -128 or ey == -128 or cy == 0: return DEC64_NAN
        # augment numerator to mimic floor(x/y), i.e. rounding towards minus infinity
        let delta =
            if (cx xor cy) >= 0: 0'i64
            elif y > 0: 1'i64
            else: -1'i64
        return Dec64(((cx + delta) div cy) shl 8)
    return floor(x / y)


proc `mod`*(x, y: Dec64): Dec64 =
    let ex = x.exp
    let ey = y.exp
    if ex == ey:
        let cx = x.coef
        let cy = y.coef
        if cx == 0 and ex != -128: return DEC64_ZERO # 0 % y ~ 0, even if y == 0 or y == nan
        if ex == -128 or ey == -128 or cy == 0: return DEC64_NAN
        let rem = cx mod cy
        # augment result to mimic x mod y == x - floor(x/y)*y
        return Dec64(
            coef:
                if rem == 0: 0'i64
                else: rem + (
                    if (cx xor cy) < 0: cy
                    else: 0
                ),
            exp: ex
        )
    return x - (floor_div(x, y) * y)
    

# x*y + z
proc fma*(x, y, z: Dec64): Dec64 =
    # Multiply two dec64 numbers together, then add another number;
    # Try to do it with higher precision than 2 separate operations
    let ex = x.exp.int
    let ey = y.exp.int
    let ez = z.exp.int

    let cx = x.coef
    let cy = y.coef

    # The result is nan if one or both of the operands is nan and neither of the
    # operands is zero.
    if (cx == 0 and ex != -128) or (cy == 0 and ey != -128): return z
    if ex == -128 or ey == -128 or ez == -128: return DEC64_NAN

    let cz = z.coef

    var r_big {.noInit.}: Int128
    {.emit: [r_big, " = (__int128)", x * y, ";"].}
    var r_high {.noInit.}: int64
    {.emit: [r_high, " = (", int64, ")(", r_big, " >> 64);"].}
    var r = cast[int64](r_big)
    let e = ex + ey
    # this is the difference from dec64_multiply
    # we need one extra bit for add_slow.
    if r_high == r shr 62:
        return add_slow(r, e, cz, ez)
    
    let abs_r_high = abs(r_high)
    let delta_er =
        if abs_r_high == 0: 1
        else: (((63 - countLeadingZeroBits(abs_r_high)) * 77) shr 8) + 2
    
    # divide by the power of ten & add z
    {.emit: [r, " = (", int64, ")(", r_big, " / ", POWER[delta_er], ");"].}
    return add_slow(r, e + delta_er, cz, ez)


proc `-`*(d: Dec64): Dec64 =
    let e = d.exp
    if e == -128: return DEC64_NAN
    
    try:
        let r = -d.coef
        if r != 0: return Dec64(coef: r, exp: e)
        else: return DEC64_ZERO
    except OverflowDefect:
        # only one overflow case for x -> -x is possible, when x = 0x800...000 (2^55)
        return makeDec64(36028797018963968'i64, e)


proc abs*(d: Dec64): Dec64 =
    if d.exp == -128: return DEC64_NAN
    return
        if d.coef < 0: -d
        elif d.coef == 0: DEC64_ZERO
        else: d


proc normal*(d: Dec64): Dec64 =
    # Make the exponent as close to zero as possible without losing any signficance.
    # Usually normalization is not needed since it does not materially change the
    # value of a number.

    var e = d.exp
    if e == -128: return DEC64_NAN
    if e == 0: return d

    var c = d.coef.int64
    if c == 0: return DEC64_ZERO

    if e < 0:
        # While the exponent is less than zero, divide the coefficient by 10 and
        # increment the exponent.
        while true:
            let tmp = c div 10
            if c != tmp * 10:
                break
            c = tmp

            inc e
            if not (e < 0): break
        return Dec64(coef: c, exp: e)
    else:
        # we keep the coefficient scaled by 256 to catch the overflow earlier,
        # inside 56 coefficient
        c = c shl 8

        # While the exponent is greater than zero, multiply the coefficient by 10 and
        # decrement the exponent. If the coefficient gets too large, wrap it up.
        while true:
            try:
                c *= 10
            except OverflowDefect:
                break

            dec e
            if not (e > 0): break
        return Dec64(c or e)


proc round*(d, place: Dec64): Dec64 =
    # The place argument indicates at what decimal place to round.
    #    -2        nearest cent
    #     0        nearest integer
    #     3        nearest thousand
    #     6        nearest million
    #     9        nearest billion

    # The place should be between -16 and 16.
    let ep = place.exp
    var cp =
        if ep != 0:
            if ep == -128: 0'i64
            else:
                let p = normal(place)
                if p.exp != 0: return DEC64_NAN
                p.coef.int64
        else:
            place.coef.int64
    
    var e = d.exp.int64
    var c = d.coef.int64

    if e == -128: return DEC64_NAN
    if c == 0: return DEC64_ZERO

    # no rounding required
    if e >= cp: return d

    let is_neg = c < 0
    var abs_c = cast[uint64](abs c)
    var abs_c_scaled = 0'u64
    while true:
        {.emit: [abs_c_scaled, " = (", uint64, ")((unsigned __int128)", abs_c, " * (", uint64, ")-3689348814741910323 >> 64);"].}
        abs_c = abs_c_scaled shr 3
        
        inc e
        if not (e < cp): break
    
    # Round if necessary and return the result.
    abs_c = (abs_c_scaled shr 2) and 1
    # Restore the correct sign
    c = if is_neg: -abs_c.int64 else: abs_c.int64

    return makeDec64(c, e.int8)


proc signum*(d: Dec64): Dec64 =
    # If the number is nan, the result is nan.
    # If the number is less than zero, the result is -1.
    # If the number is zero, the result is 0.
    # If the number is greater than zero, the result is 1.
    let e = d.exp
    if e == -128: return DEC64_NAN

    if d.coef < 0:
        return DEC64_NEG_ONE
    elif d.coef == 0:
        return DEC64_ZERO
    else:
        return DEC64_ONE


proc dec64_from_double*(d: float64): Dec64 =
    const SHIFT = 18
    var (m, e2) = frexp(d)
    let e10 = e2.float64 * 0.3010299956639811952137388947'f64
    let e = ceil(e10).int
    m *= pow(10'f64, e10 - e.float64)
    let m64 = round(m * 1000000000000000000'f64).int64
    # still has some rounding issues
    return normal round(makeDec64(m64, e - SHIFT), Dec64(coef: (e-15) * 256, exp: 0))


proc acos*(slope: Dec64): Dec64 =
    return DEC64_HALF_PI - asin(slope)


proc asin*(slope: Dec64): Dec64 =
    if slope == DEC64_ONE:
        return DEC64_HALF_PI
    if slope == DEC64_NEG_ONE:
        return DEC64_NHALF_PI
    if is_nan(slope) or DEC64_ONE < abs(slope):
        return DEC64_NAN

    var bottom = DEC64_2
    var factor = slope
    var x2 = slope * slope
    result = factor
    while true:
        factor = (((DEC64_NEG_ONE + bottom) * x2) * factor) / bottom
        let progress = result + (factor / (DEC64_ONE + bottom))
        if result == progress:
            break
        result = progress
        bottom += DEC64_2


proc atan*(slope: Dec64): Dec64 =
    var d = slope
    var rev = false
    var neg = false
    if slope.coef < 0:
        d = -d
        neg = true
    if DEC64_ONE < d:
        d = DEC64_ONE / d
        rev = true
    
    var a = asin(d / sqrt(DEC64_ONE + (d * d)))
    if rev: a = DEC64_HALF_PI - a
    if neg: a = -a
    return a



proc atan2*(y, x: Dec64): Dec64 =
    if is_zero x:
        if is_zero y:
            return DEC64_NAN
        elif y.coef < 0:
            return DEC64_NHALF_PI
        else:
            return DEC64_HALF_PI
    else:
        let a = atan(y / x)
        if x.coef < 0:
            if y.coef < 0:
                return a - DEC64_HALF_PI
            else:
                return a + DEC64_HALF_PI
        else:
            return a


proc cos*(radians: Dec64): Dec64 =
    return sin(radians + DEC64_HALF_PI)


const
    DEC64_EXP_N = 256
    EXP_TAB = [ # exp(-i*log(10)/256), i=0..256
        Dec64 2560000000000000240, Dec64 2537077391997084144, Dec64 2514360036321377008, Dec64 2491846095114035952,
        Dec64 2469533746972670192, Dec64 2447421186803987696, Dec64 2425506625677761264, Dec64 2403788290682102256,
        Dec64 2382264424780029680, Dec64 2360933286667324400, Dec64 2339793150631655920, Dec64 2318842306412969200,
        Dec64 2298079059065124592, Dec64 2277501728818772976, Dec64 2257108650945461744, Dec64 2236898175622956272,
        Dec64 2216868667801767664, Dec64 2197018507072874224, Dec64 2177346087536630512, Dec64 2157849817672847344,
        Dec64 2138528120212035312, Dec64 2119379432007803376, Dec64 2100402203910397936, Dec64 2081594900641374704,
        Dec64 2062956000669393648, Dec64 2044483996087124976, Dec64 2026177392489257456, Dec64 2008034708851600368,
        Dec64 1990054477411266288, Dec64 1972235243547927792, Dec64 1954575565666136816, Dec64 1937074015078697968,
        Dec64 1919729175891087088, Dec64 1902539644886902512, Dec64 1885504031414343920, Dec64 1868620957273707504,
        Dec64 1851889056605887728, Dec64 1835306975781876976, Dec64 1818873373293257712, Dec64 1802586919643670512,
        Dec64 1786446297241257968, Dec64 1770450200292069616, Dec64 1754597334694421488, Dec64 1738886417934201840,
        Dec64 1723316178981115632, Dec64 1707885358185854960, Dec64 1692592707178193136, Dec64 1677436988765989872,
        Dec64 1662416976835101168, Dec64 1647531456250185712, Dec64 1632779222756397808, Dec64 1618159082881963760,
        Dec64 1603669853841627376, Dec64 1589310363440961008, Dec64 1575079449981535472, Dec64 1560975962166935792,
        Dec64 1546998759009620208, Dec64 1533146709738614256, Dec64 1519418693708028656, Dec64 1505813600306398960,
        Dec64 1492330328866835184, Dec64 1478967788577976816, Dec64 1465724898395745264, Dec64 1452600586955886064,
        Dec64 1439593792487293936, Dec64 1426703462726115056, Dec64 1413928554830617584, Dec64 1401268035296823792,
        Dec64 1388720879874899696, Dec64 1376286073486291440, Dec64 1363962610141604336, Dec64 1351749492859217392,
        Dec64 1339645733584626672, Dec64 1327650353110509552, Dec64 1315762380997507568, Dec64 1303980855495714800,
        Dec64 1292304823466872816, Dec64 1280733340307259888, Dec64 1269265469871272176, Dec64 1257900284395687664,
        Dec64 1246636864424609776, Dec64 1235474298735082480, Dec64 1224411684263370736, Dec64 1213448126031902192,
        Dec64 1202582737076862192, Dec64 1191814638376437488, Dec64 1181142958779703024, Dec64 1170566834936143856,
        Dec64 1160085411225809648, Dec64 1149697839690094320, Dec64 1139403279963135216, Dec64 1129200899203825904,
        Dec64 1119089872028439280, Dec64 1109069380443852784, Dec64 1099138613781371632, Dec64 1089296768631145456,
        Dec64 1079543048777170672, Dec64 1069876665132877040, Dec64 1060296835677287920, Dec64 1050802785391755504,
        Dec64 1041393746197260272, Dec64 1032068956892272880, Dec64 1022827663091171056, Dec64 1013669117163210992,
        Dec64 1004592578172041456, Dec64 995597311815761392, Dec64 986682590367515376, Dec64 977847692616618480,
        Dec64 969091903810209520, Dec64 960414515595427824, Dec64 951814825962105584, Dec64 943292139185975536,
        Dec64 934845765772384752, Dec64 926475022400515056, Dec64 9181792318681008367, Dec64 9099577230366433519,
        Dec64 9018098307771139823, Dec64 8937348959161452527, Dec64 8857322651827027183, Dec64 8778012911552347375,
        Dec64 8699413322092952303, Dec64 8621517524656357103, Dec64 8544319217387618799, Dec64 8467812154859509487,
        Dec64 8391990147567254255, Dec64 8316847061427793903, Dec64 8242376817283530479, Dec64 8168573390410516975,
        Dec64 8095430810031051247, Dec64 8022943158830632687, Dec64 7951104572479244783, Dec64 7879909239156926191,
        Dec64 7809351399083585519, Dec64 7739425344053031663, Dec64 7670125416971171567, Dec64 7601446011398345711,
        Dec64 7533381571095761647, Dec64 7465926589575987951, Dec64 7399075609657472495, Dec64 7332823223023052271,
        Dec64 7267164069782412527, Dec64 7202092838038467567, Dec64 7137604263457621487, Dec64 7073693128843878383,
        Dec64 7010354263716765167, Dec64 6947582543893034223, Dec64 6885372891072111087, Dec64 6823720272425254895,
        Dec64 6762619700188397039, Dec64 6702066231258624751, Dec64 6642054966794279663, Dec64 6582581051818636015,
        Dec64 6523639674827127279, Dec64 6465226067398090735, Dec64 6407335503806996207, Dec64 6349963300644131055,
        Dec64 6293104816435705583, Dec64 6236755451268354287, Dec64 6180910646416996847, Dec64 6125565883976032751,
        Dec64 6070716686493837807, Dec64 6016358616610532847, Dec64 5962487276698997743, Dec64 5909098308509096687,
        Dec64 5856187392815093231, Dec64 5803750249066218223, Dec64 5751782635040369647, Dec64 5700280346500912367,
        Dec64 5649239216856550127, Dec64 5598655116824243951, Dec64 5548523954095149039, Dec64 5498841673003542255,
        Dec64 5449604254198714095, Dec64 5400807714319799535, Dec64 5352448105673518831, Dec64 5304521515914805743,
        Dec64 5257024067730294255, Dec64 5209951918524638959, Dec64 5163301260109646319, Dec64 5117068318396186607,
        Dec64 5071249353088867567, Dec64 5025840657383438319, Dec64 4980838557666907631, Dec64 4936239413220341487,
        Dec64 4892039615924327919, Dec64 4848235589967074031, Dec64 4804823791555120111, Dec64 4761800708626642159,
        Dec64 4719162860567323119, Dec64 4676906797928766191, Dec64 4635029102149432303, Dec64 4593526385278073071,
        Dec64 4552395289699642607, Dec64 4511632487863663343, Dec64 4471234682015022575, Dec64 4431198603927181295,
        Dec64 4391521014637770223, Dec64 4352198704186555375, Dec64 4313228491355747567, Dec64 4274607223412639215,
        Dec64 4236331775854544623, Dec64 4198399052156024559, Dec64 4160805983518373359, Dec64 4123549528621349871,
        Dec64 4086626673377132271, Dec64 4050034430686473967, Dec64 4013769840197044463, Dec64 3977829968063933423,
        Dec64 3942211906712299759, Dec64 3906912774602145519, Dec64 3871929715995196143, Dec64 3837259900723867887,
        Dec64 3802900523962304495, Dec64 3768848805999463663, Dec64 3735101992014235375, Dec64 3701657351852573679,
        Dec64 3668512179806624751, Dec64 3635663794395831535, Dec64 3603109538149999343, Dec64 3570846777394303727,
        Dec64 3538872902036223215, Dec64 3507185325354380015, Dec64 3475781483789271279, Dec64 3444658836735874543,
        Dec64 3413814866338109679, Dec64 3383247077285142511, Dec64 3352952996609510127, Dec64 3322930173487055855,
        Dec64 3293176179038654959, Dec64 3263688606133715695, Dec64 3234465069195439599, Dec64 3205503204007825391,
        Dec64 3176800667524402415, Dec64 3148355137678674159, Dec64 3120164313196261871, Dec64 3092225913408728303,
        Dec64 3064537678069069039, Dec64 3037097367168856303, Dec64 3009902760757020143, Dec64 2982951658760251375,
        Dec64 2956241880805013231, Dec64 2929771266041147119, Dec64 2903537672967057903, Dec64 2877538979256462831,
        Dec64 2851773081586694383, Dec64 2826237895468538095, Dec64 2800931355077595887, Dec64 2775851413087158255,
        Dec64 2750996040502573039, Dec64 2726363226497098479, Dec64 2701950978249223919, Dec64 2677757320781449455,
        Dec64 2653780296800507119, Dec64 2630017966539014639, Dec64 2606468407598545647, Dec64 2583129714794105583,
        Dec64 2560000000000000239
    ]

proc exp*(d: Dec64): Dec64 =
    const
        log10 = Dec64(coef: 23025850929940457, exp: -16)
        mlog10 = Dec64(coef: -23025850929940457, exp: -16)
        m256_div_log10 = Dec64(coef: 11117938736723247, exp: -14)
        log10_div_256 = Dec64(coef: 8994473019507991, exp: -18)
    
    if is_nan d: return DEC64_NAN
    
    let e10 = normal ceil(d / log10)
    let e = e10.exp
    if e >= 200: return DEC64_NAN
    if e <= -200: return DEC64_ZERO
    var y = fma(e10, mlog10, d)
    let y_tab = normal floor(y * m256_div_log10)
    var tab_idx = clamp(y_tab.coef.int64, 0, DEC64_EXP_N)
    y = fma(Dec64(coef: tab_idx, exp: 0), log10_div_256, y)

    let scale = EXP_TAB[tab_idx]
    var p = scale
    var s = scale

    for n in times(30):
        p *= y / Dec64(n * 256)
        let progress = s + p
        if progress == s:
            break
        s = progress
    
    if is_nan s:
        return DEC64_NAN

    return makeDec64(s.coef, s.exp + e)


const
    FAC = 93
    FACTORIALS = [
        Dec64 (1'i64 shl 8) + 0,
        Dec64 (1'i64 shl 8) + 0,
        Dec64 (2'i64 shl 8) + 0,
        Dec64 (6'i64 shl 8) + 0,
        Dec64 (24'i64 shl 8) + 0,
        Dec64 (120'i64 shl 8) + 0,
        Dec64 (720'i64 shl 8) + 0,
        Dec64 (5040'i64 shl 8) + 0,
        Dec64 (40320'i64 shl 8) + 0,
        Dec64 (362880'i64 shl 8) + 0,
        Dec64 (3628800'i64 shl 8) + 0,
        Dec64 (39916800'i64 shl 8) + 0,
        Dec64 (479001600'i64 shl 8) + 0,
        Dec64 (6227020800'i64 shl 8) + 0,
        Dec64 (87178291200'i64 shl 8) + 0,
        Dec64 (1307674368000'i64 shl 8) + 0,
        Dec64 (20922789888000'i64 shl 8) + 0,
        Dec64 (355687428096000'i64 shl 8) + 0,
        Dec64 (6402373705728000'i64 shl 8) + 0,
        Dec64 (12164510040883200'i64 shl 8) + 1,
        Dec64 (24329020081766400'i64 shl 8) + 2,
        Dec64 (5109094217170944'i64 shl 8) + 4,
        Dec64 (11240007277776077'i64 shl 8) + 5,
        Dec64 (25852016738884977'i64 shl 8) + 6,
        Dec64 (6204484017332394'i64 shl 8) + 8,
        Dec64 (15511210043330986'i64 shl 8) + 9,
        Dec64 (4032914611266056'i64 shl 8) + 11,
        Dec64 (10888869450418352'i64 shl 8) + 12,
        Dec64 (30488834461171386'i64 shl 8) + 13,
        Dec64 (8841761993739702'i64 shl 8) + 15,
        Dec64 (26525285981219106'i64 shl 8) + 16,
        Dec64 (8222838654177923'i64 shl 8) + 18,
        Dec64 (26313083693369353'i64 shl 8) + 19,
        Dec64 (8683317618811886'i64 shl 8) + 21,
        Dec64 (29523279903960414'i64 shl 8) + 22,
        Dec64 (10333147966386145'i64 shl 8) + 24,
        Dec64 (3719933267899012'i64 shl 8) + 26,
        Dec64 (13763753091226345'i64 shl 8) + 27,
        Dec64 (5230226174666011'i64 shl 8) + 29,
        Dec64 (20397882081197443'i64 shl 8) + 30,
        Dec64 (8159152832478977'i64 shl 8) + 32,
        Dec64 (33452526613163807'i64 shl 8) + 33,
        Dec64 (14050061177528799'i64 shl 8) + 35,
        Dec64 (6041526306337384'i64 shl 8) + 37,
        Dec64 (26582715747884488'i64 shl 8) + 38,
        Dec64 (11962222086548019'i64 shl 8) + 40,
        Dec64 (5502622159812089'i64 shl 8) + 42,
        Dec64 (25862324151116818'i64 shl 8) + 43,
        Dec64 (12413915592536073'i64 shl 8) + 45,
        Dec64 (6082818640342676'i64 shl 8) + 47,
        Dec64 (30414093201713378'i64 shl 8) + 48,
        Dec64 (15511187532873823'i64 shl 8) + 50,
        Dec64 (8065817517094388'i64 shl 8) + 52,
        Dec64 (4274883284060026'i64 shl 8) + 54,
        Dec64 (23084369733924138'i64 shl 8) + 55,
        Dec64 (12696403353658276'i64 shl 8) + 57,
        Dec64 (7109985878048635'i64 shl 8) + 59,
        Dec64 (4052691950487722'i64 shl 8) + 61,
        Dec64 (23505613312828786'i64 shl 8) + 62,
        Dec64 (13868311854568984'i64 shl 8) + 64,
        Dec64 (8320987112741390'i64 shl 8) + 66,
        Dec64 (5075802138772248'i64 shl 8) + 68,
        Dec64 (31469973260387938'i64 shl 8) + 69,
        Dec64 (19826083154044401'i64 shl 8) + 71,
        Dec64 (12688693218588416'i64 shl 8) + 73,
        Dec64 (8247650592082471'i64 shl 8) + 75,
        Dec64 (5443449390774431'i64 shl 8) + 77,
        Dec64 (3647111091818869'i64 shl 8) + 79,
        Dec64 (24800355424368306'i64 shl 8) + 80,
        Dec64 (17112245242814131'i64 shl 8) + 82,
        Dec64 (11978571669969892'i64 shl 8) + 84,
        Dec64 (8504785885678623'i64 shl 8) + 86,
        Dec64 (6123445837688609'i64 shl 8) + 88,
        Dec64 (4470115461512684'i64 shl 8) + 90,
        Dec64 (33078854415193864'i64 shl 8) + 91,
        Dec64 (24809140811395398'i64 shl 8) + 93,
        Dec64 (18854947016660503'i64 shl 8) + 95,
        Dec64 (14518309202828587'i64 shl 8) + 97,
        Dec64 (11324281178206298'i64 shl 8) + 99,
        Dec64 (8946182130782975'i64 shl 8) + 101,
        Dec64 (7156945704626380'i64 shl 8) + 103,
        Dec64 (5797126020747368'i64 shl 8) + 105,
        Dec64 (4753643337012842'i64 shl 8) + 107,
        Dec64 (3945523969720659'i64 shl 8) + 109,
        Dec64 (33142401345653533'i64 shl 8) + 110,
        Dec64 (28171041143805503'i64 shl 8) + 112,
        Dec64 (24227095383672732'i64 shl 8) + 114,
        Dec64 (21077572983795277'i64 shl 8) + 116,
        Dec64 (18548264225739844'i64 shl 8) + 118,
        Dec64 (16507955160908461'i64 shl 8) + 120,
        Dec64 (14857159644817615'i64 shl 8) + 122,
        Dec64 (1352001527678403'i64 shl 8) + 124,
        Dec64 (12438414054641307'i64 shl 8) + 126
    ]

proc factorial*(d: Dec64): Dec64 =
    let n = normal d
    let c = n.coef
    if c >= 0 and c < FAC and n.exp == 0:
        return FACTORIALS[c.int]
    return DEC64_NAN


const
    LOG_TAB_A = -950
    LOG_TAB_B = 1000
    LOG_TAB = [ # log(1+i*0.001), i=LOGTAB_A..LOGTAB_B
        Dec64 -7669074620298216720,  Dec64 -7618379894419996688,  Dec64 -7568669594625816592,  Dec64 -7519906215500838672,  Dec64 -7472054354989728272,
        Dec64 -7425080559999144976,  Dec64 -7378953185912288528,  Dec64 -7333642268537822224,  Dec64 -7289119407195437072,  Dec64 -7245357657795628816,
        Dec64 -7202331434905692944,  Dec64 -7160016421910593808,  Dec64 -7118389488478836240,  Dec64 -7077428614631947024,  Dec64 -7037112820793510672,
        Dec64 -6997422103261439504,  Dec64 -6958337374606621200,  Dec64 -6919840408553397520,  Dec64 -6881913788943437584,  Dec64 -6844540862425286672,
        Dec64 -6807705694547911696,  Dec64 -6771393028968503056,  Dec64 -6735588249513168912,  Dec64 -6700277344854389264,  Dec64 -6665446875591596048,
        Dec64 -6631083943541315856,  Dec64 -6597176163061263120,  Dec64 -6563711634248839952,  Dec64 -6530678917868915728,  Dec64 -6498067011878695696,
        Dec64 -6465865329429133584,  Dec64 -6434063678232827408,  Dec64 -6402652241197782544,  Dec64 -6371621558234979856,  Dec64 -6340962509155387664,
        Dec64 -6310666297579060496,  Dec64 -6280724435785290768,  Dec64 -6251128730438536208,  Dec64 -6221871269130062096,  Dec64 -6192944407679992592,
        Dec64 -6164340758148792080,  Dec64 -6136053177511134480,  Dec64 -6108074756948727312,  Dec64 -6080398811721935376,  Dec64 -6053018871583060752,
        Dec64 -6025928671696886032,  Dec64 -5999122144036609808,  Dec64 -5972593409225610512,  Dec64 -5946336768797606416,  Dec64 -5920346697849720336,
        Dec64 -5894617838064756752,  Dec64 -5869144991080646416,  Dec64 -5843923112186536720,  Dec64 -5818947304326403088,  Dec64 -5794212812392356624,
        Dec64 -5769715017791010832,  Dec64 -5745449433267378704,  Dec64 -5721411697971790864,  Dec64 -5697597572756268304,  Dec64 -5674002935687662864,
        Dec64 -5650623777765685008,  Dec64 -5627456198834695184,  Dec64 -5604496403678828560,  Dec64 -5581740698290678800,  Dec64 -5559185486304362256,
        Dec64 -5536827265584350480,  Dec64 -5514662624961977104,  Dec64 -5492688241112014864,  Dec64 -5470900875562168848,  Dec64 -5449297371828755472,
        Dec64 -5427874652672232976,  Dec64 -5406629717466613520,  Dec64 -5385559639677133840,  Dec64 -5364661564440881936,  Dec64 -5343932706245376272,
        Dec64 -5323370346700379664,  Dec64 -5302971832398487056,  Dec64 -5282734572860277008,  Dec64 -5262656038560050704,  Dec64 -5242733759028389904,
        Dec64 -5222965321027979536,  Dec64 -5203348366799322640,  Dec64 -5183880592373161232,  Dec64 -5164559745946581008,  Dec64 -5145383626319937552,
        Dec64 -5126350081391891216,  Dec64 -5107457006709977360,  Dec64 -5088702344074270736,  Dec64 -5070084080191826704,  Dec64 -5051600245379699728,
        Dec64 -5033248912314451728,  Dec64 -5015028194826159888,  Dec64 -4996936246735043088,  Dec64 -4978971260728908048,  Dec64 -4961131467279708944,
        Dec64 -4943415133597600272,  Dec64 -4925820562620929296,  Dec64 -4908346092040705808,  Dec64 -4890990093358136080,  Dec64 -4873750970973895184,
        Dec64 -4856627161307855888,  Dec64 -4839617131948064272,  Dec64 -4822719380827803152,  Dec64 -4805932435429635856,  Dec64 -4789254852015379984,
        Dec64 -4772685214880999184,  Dec64 -4756222135635455760,  Dec64 -4739864252502602000,  Dec64 -4723610229645235472,  Dec64 -4707458756510477840,
        Dec64 -4691408547195673616,  Dec64 -4675458339834045456,  Dec64 -4659606895999367440,  Dec64 -4643853000128958992,  Dec64 -4628195458964322576,
        Dec64 -4612633101008784144,  Dec64 -4597164776001519888,  Dec64 -4581789354407377424,  Dec64 -4566505726921927696,  Dec64 -4551312803991202576,
        Dec64 -4536209515345600528,  Dec64 -4521194809547461392,  Dec64 -4506267653551830800,  Dec64 -4491427032279956496,  Dec64 -4476671948205076240,
        Dec64 -4462001420950074640,  Dec64 -4447414486896602128,  Dec64 -4432910198805267984,  Dec64 -4418487625446532624,  Dec64 -4404145851241937936,
        Dec64 -4389883975915332112,  Dec64 -4375701114153756688,  Dec64 -4361596395277674512,  Dec64 -4347568962920232976,  Dec64 -4333617974715267344,
        Dec64 -4319742601993758992,  Dec64 -4305942029488475408,  Dec64 -4292215455046528784,  Dec64 -4278562089349600528,  Dec64 -4264981155641586192,
        Dec64 -4251471889463426064,  Dec64 -4238033538394898192,  Dec64 -4224665361803149840,  Dec64 -4211366630597763600,  Dec64 -4198136626992150544,
        Dec64 -4184974644271078928,  Dec64 -4171879986564146448,  Dec64 -4158851968625019920,  Dec64 -4145889915616260368,  Dec64 -4132993162899570192,
        Dec64 -4120161055831296784,  Dec64 -4107392949563036688,  Dec64 -4094688208847186448,  Dec64 -4082046207847294992,  Dec64 -4069466329953076752,
        Dec64 -4056947967599945744,  Dec64 -4044490522092943120,  Dec64 -4032093403434925840,  Dec64 -4019756030158896656,  Dec64 -4007477829164354320,
        Dec64 -3995258235557550864,  Dec64 -3983096692495540496,  Dec64 -3970992651033918736,  Dec64 -3958945569978142224,  Dec64 -3946954915738330896,
        Dec64 -3935020162187453968,  Dec64 -3923140790522808080,  Dec64 -3911316289130694160,  Dec64 -3899546153454202640,  Dec64 -3887829885864028432,
        Dec64 -3876166995532225040,  Dec64 -3864556998308823312,  Dec64 -3852999416601235216,  Dec64 -3841493779256366608,  Dec64 -3830039621445368592,
        Dec64 -3818636484550955024,  Dec64 -3807283916057218832,  Dec64 -3795981469441879824,  Dec64 -3784728704070902288,  Dec64 -3773525185095417104,
        Dec64 -3762370483350890512,  Dec64 -3751264175258479120,  Dec64 -3740205842728517136,  Dec64 -3729195073066077200,  Dec64 -3718231458878554896,
        Dec64 -3707314597985223696,  Dec64 -3696444093328708880,  Dec64 -3685619552888334864,  Dec64 -3674840589595295504,  Dec64 -3664106821249603344,
        Dec64 -3653417870438772752,  Dec64 -3642773364458193936,  Dec64 -3632172935233153552,  Dec64 -3621616219242466576,  Dec64 -3611102857443673872,
        Dec64 -3600632495199769616,  Dec64 -3590204782207421712,  Dec64 -3579819372426649104,  Dec64 -3569475924011916304,  Dec64 -3559174099244619024,
        Dec64 -3548913564466919696,  Dec64 -3538693990016903952,  Dec64 -3528515050165026832,  Dec64 -3518376423051818768,  Dec64 -3508277790626817040,
        Dec64 -3498218838588699664,  Dec64 -3488199256326590736,  Dec64 -3478218736862508048,  Dec64 -3468276976794929936,  Dec64 -3458373676243453968,
        Dec64 -3448508538794519568,  Dec64 -3438681271448175376,  Dec64 -3428891584565862672,  Dec64 -3419139191819193360,  Dec64 -3409423810139701264,
        Dec64 -3399745159669541648,  Dec64 -3390102963713121040,  Dec64 -3380496948689631760,  Dec64 -3370926844086477584,  Dec64 -3361392382413562384,
        Dec64 -3351893299158431248,  Dec64 -3342429332742236432,  Dec64 -3333000224476517392,  Dec64 -3323605718520773648,  Dec64 -3314245561840810768,
        Dec64 -3304919504167848208,  Dec64 -3295627297958366736,  Dec64 -3286368698354684176,  Dec64 -3277143463146239760,  Dec64 -3267951352731574544,
        Dec64 -3258792130080991504,  Dec64 -3249665560699881488,  Dec64 -3240571412592699920,  Dec64 -3231509456227582224,  Dec64 -3222479464501583120,
        Dec64 -3213481212706525200,  Dec64 -3204514478495448080,  Dec64 -3195579041849640464,  Dec64 -3186674685046248976,  Dec64 -3177801192626444304,
        Dec64 -3168958351364140048,  Dec64 -3160145950235249680,  Dec64 -3151363780387469328,  Dec64 -3142611635110578192,  Dec64 -3133889309807245584,
        Dec64 -3125196601964331792,  Dec64 -3116533311124676112,  Dec64 -3107899238859359504,  Dec64 -3099294188740435216,  Dec64 -3090717966314113296,
        Dec64 -3082170379074395920,  Dec64 -3073651236437148688,  Dec64 -3065160349714604304,  Dec64 -3056697532090285584,  Dec64 -3048262598594343184,
        Dec64 -3039855366079296784,  Dec64 -3031475653196175888,  Dec64 -3023123280371045904,  Dec64 -3014798069781920016,  Dec64 -3006499845336042256,
        Dec64 -2998228432647539216,  Dec64 -2989983659015431184,  Dec64 -2981765353401995792,  Dec64 -2973573346411478288,  Dec64 -2965407470269142032,
        Dec64 -2957267558800650000,  Dec64 -2949153447411775504,  Dec64 -2941064973068432144,  Dec64 -2933001974277017872,  Dec64 -2924964291065068560,
        Dec64 -2916951764962213648,  Dec64 -2908964238981430032,  Dec64 -2901001557600585232,  Dec64 -2893063566744269840,  Dec64 -2885150113765907472,
        Dec64 -2877261047430142736,  Dec64 -2869396217895499024,  Dec64 -2861555476697302032,  Dec64 -2853738676730862608,  Dec64 -2845945672234918672,
        Dec64 -2838176318775324176,  Dec64 -2830430473228989712,  Dec64 -2822707993768059920,  Dec64 -2815008739844334352,  Dec64 -2807332572173917456,
        Dec64 -2799679352722100496,  Dec64 -2792048944688467728,  Dec64 -2784441212492224528,  Dec64 -2776856021757742608,  Dec64 -2769293239300317968,
        Dec64 -2761752733112140560,  Dec64 -2754234372348467728,  Dec64 -2746738027314001424,  Dec64 -2739263569449464336,  Dec64 -2731810871318370832,
        Dec64 -2724379806593989648,  Dec64 -2716970250046496528,  Dec64 -2709582077530311184,  Dec64 -2702215165971616272,  Dec64 -2694869393356057104,
        Dec64 -2687544638716614672,  Dec64 -2680240782121654032,  Dec64 -2672957704663142160,  Dec64 -2665695288445030928,  Dec64 -2658453416571808016,
        Dec64 -2651231973137206288,  Dec64 -2644030843213072656,  Dec64 -2636849912838394640,  Dec64 -2629689069008477712,  Dec64 -2622548199664276752,
        Dec64 -2615427193681872144,  Dec64 -2608325940862095376,  Dec64 -2601244331920296720,  Dec64 -2594182258476252688,  Dec64 -2587139613044214544,
        Dec64 -2580116289023092240,  Dec64 -2573112180686773008,  Dec64 -2566127183174570768,  Dec64 -2559161192481807376,  Dec64 -2552214105450521104,
        Dec64 -2545285819760299024,  Dec64 -2538376233919236624,  Dec64 -2531485247255015440,  Dec64 -2524612759906102800,  Dec64 -2517758672813068816,
        Dec64 -2510922887710018832,  Dec64 -2504105307116140560,  Dec64 -2497305834327363088,  Dec64 -2490524373408126224,  Dec64 -2483760829183259152,
        Dec64 -2477015107229966096,  Dec64 -2470287113869916176,  Dec64 -2463576756161438224,  Dec64 -2456883941891816976,  Dec64 -2450208579569689872,
        Dec64 -2443550578417542928,  Dec64 -2436909848364303632,  Dec64 -2430286300038029072,  Dec64 -2423679844758690576,  Dec64 -2417090394531048208,
        Dec64 -2410517862037618704,  Dec64 -2403962160631734288,  Dec64 -2397423204330686480,  Dec64 -2390900907808961808,  Dec64 -2384395186391559952,
        Dec64 -2377905956047398672,  Dec64 -2371433133382800400,  Dec64 -2364976635635063056,  Dec64 -2358536380666109968,  Dec64 -2352112286956220176,
        Dec64 -2345704273597836816,  Dec64 -2339312260289453584,  Dec64 -2332936167329576720,  Dec64 -2326575915610762256,  Dec64 -2320231426613726480,
        Dec64 -2313902622401530384,  Dec64 -2307589425613835024,  Dec64 -2301291759461227536,  Dec64 -2295009547719616784,  Dec64 -2288742714724698128,
        Dec64 -2282491185366485776,  Dec64 -2276254885083909904,  Dec64 -2270033739859483152,  Dec64 -2263827676214026768,  Dec64 -2257636621201465872,
        Dec64 -2251460502403683088,  Dec64 -2245299247925436688,  Dec64 -2239152786389338896,  Dec64 -2233021046930894352,  Dec64 -2226903959193597968,
        Dec64 -2220801453324090896,  Dec64 -2214713459967374096,  Dec64 -2208639910262080272,  Dec64 -2202580735835799056,  Dec64 -2196535868800458768,
        Dec64 -2190505241747763472,  Dec64 -2184488787744682256,  Dec64 -2178486440328991760,  Dec64 -2172498133504870928,  Dec64 -2166523801738547216,
        Dec64 -2160563379953993744,  Dec64 -2154616803528676368,  Dec64 -2148684008289348112,  Dec64 -2142764930507896592,  Dec64 -2136859506897234192,
        Dec64 -2130967674607239440,  Dec64 -2125089371220742672,  Dec64 -2119224534749559824,  Dec64 -2113373103630568464,  Dec64 -2107535016721831952,
        Dec64 -2101710213298765072,  Dec64 -2095898633050344976,  Dec64 -2090100216075363344,  Dec64 -2084314902878723344,  Dec64 -2078542634367775248,
        Dec64 -2072783351848695568,  Dec64 -2067036997022906640,  Dec64 -2061303511983534352,  Dec64 -2055582839211908624,  Dec64 -2049874921574099728,
        Dec64 -2044179702317495056,  Dec64 -2038497125067414032,  Dec64 -2032827133823758864,  Dec64 -2027169672957703440,  Dec64 -2021524687208419856,
        Dec64 -2015892121679837456,  Dec64 -2010271921837442320,  Dec64 -2004664033505106704,  Dec64 -1999068402861957136,  Dec64 -1993484976439275024,
        Dec64 -1987913701117430544,  Dec64 -1982354524122850320,  Dec64 -1976807393025019152,  Dec64 -1971272255733511440,  Dec64 -1965749060495057168,
        Dec64 -1960237755890638352,  Dec64 -1954738290832617232,  Dec64 -1949250614561893392,  Dec64 -1943774676645094928,  Dec64 -1938310426971795472,
        Dec64 -1932857815751763728,  Dec64 -1927416793512241168,  Dec64 -1921987311095248912,  Dec64 -1916569319654922256,  Dec64 -1911162770654874896,
        Dec64 -1905767615865589008,  Dec64 -1900383807361835280,  Dec64 -1895011297520117008,  Dec64 -1889650039016143376,  Dec64 -1884299984822327568,
        Dec64 -1878961088205312784,  Dec64 -1873633302723522064,  Dec64 -1868316582224733712,  Dec64 -1863010880843684624,  Dec64 -1857716152999693328,
        Dec64 -1852432353394313744,  Dec64 -1847159437009006608,  Dec64 -1841897359102840848,  Dec64 -1836646075210213904,  Dec64 -1831405541138598160,
        Dec64 -1826175712966309648,  Dec64 -1820956547040298000,  Dec64 -1815747999973961744,  Dec64 -1810550028644983824,  Dec64 -1805362590193188880,
        Dec64 -1800185642018423568,  Dec64 -1795019141778456336,  Dec64 -1789863047386901008,  Dec64 -1784717317011159056,  Dec64 -1779581909070382864,
        Dec64 -1774456782233459728,  Dec64 -1769341895417016848,  Dec64 -1764237207783443984,  Dec64 -1759142678738938128,  Dec64 -1754058267931566864,
        Dec64 -1748983935249349392,  Dec64 -1743919640818358800,  Dec64 -1738865345000841744,  Dec64 -1733821008393357072,  Dec64 -1728786591824932368,
        Dec64 -1723762056355239696,  Dec64 -1718747363272787216,  Dec64 -1713742474093130768,  Dec64 -1708747350557100560,  Dec64 -1703761954629047824,
        Dec64 -1698786248495106064,  Dec64 -1693820194561469968,  Dec64 -1688863755452691984,  Dec64 -1683916894009994000,  Dec64 -1678979573289595664,
        Dec64 -1674051756561059600,  Dec64 -1669133407305651216,  Dec64 -1664224489214715408,  Dec64 -1659324966188068112,  Dec64 -1654434802332402704,
        Dec64 -1649553961959713808,  Dec64 -1644682409585733392,  Dec64 -1639820109928382992,  Dec64 -1634967027906241296,  Dec64 -1630123128637024272,
        Dec64 -1625288377436081680,  Dec64 -1620462739814907152,  Dec64 -1615646181479661072,  Dec64 -1610838668329708560,  Dec64 -1606040166456171792,
        Dec64 -1601250642140493840,  Dec64 -1596470061853017616,  Dec64 -1591698392251577104,  Dec64 -1586935600180102416,  Dec64 -1582181652667237904,
        Dec64 -1577436516924971280,  Dec64 -1572700160347277840,  Dec64 -1567972550508776208,  Dec64 -1563253655163395856,  Dec64 -1558543442243057424,
        Dec64 -1553841879856365840,  Dec64 -1549148936287313680,  Dec64 -1544464579993998608,  Dec64 -1539788779607350800,  Dec64 -1535121503929872144,
        Dec64 -1530462721934387984,  Dec64 -1525812402762809616,  Dec64 -1521170515724906512,  Dec64 -1516537030297093648,  Dec64 -1511911916121223952,
        Dec64 -1507295143003398160,  Dec64 -1502686680912779792,  Dec64 -1498086499980423440,  Dec64 -1493494570498114576,  Dec64 -1488910862917216784,
        Dec64 -1484335347847531536,  Dec64 -1479767996056167952,  Dec64 -1475208778466421520,  Dec64 -1470657666156663312,  Dec64 -1466114630359239952,
        Dec64 -1461579642459381776,  Dec64 -1457052673994122256,  Dec64 -1452533696651225360,  Dec64 -1448022682268123152,  Dec64 -1443519602830863376,
        Dec64 -1439024430473065232,  Dec64 -1434537137474884368,  Dec64 -1430057696261988112,  Dec64 -1425586079404537360,  Dec64 -1421122259616180496,
        Dec64 -1416666209753053456,  Dec64 -1412217902812789008,  Dec64 -1407777311933535760,  Dec64 -1403344410392984336,  Dec64 -1398919171607402768,
        Dec64 -1394501569130680080,  Dec64 -1390091576653377808,  Dec64 -1385689168001789712,  Dec64 -1381294317137010192,  Dec64 -1376906998154009360,
        Dec64 -1372527185280718096,  Dec64 -1368154852877118224,  Dec64 -1363789975434343696,  Dec64 -1359432527573785616,  Dec64 -1355082484046208528,
        Dec64 -1350739819730871824,  Dec64 -1346404509634659344,  Dec64 -1342076528891216144,  Dec64 -1337755852760093456,  Dec64 -1333442456625899536,
        Dec64 -1329136315997458448,  Dec64 -1324837406506975248,  Dec64 -1320545703909209360,  Dec64 -1316261184080653328,  Dec64 -1311983823018720272,
        Dec64 -1307713596840935952,  Dec64 -1303450481784139280,  Dec64 -1299194454203688720,  Dec64 -1294945490572675856,  Dec64 -1290703567481144336,
        Dec64 -1286468661635316496,  Dec64 -1282240749856825616,  Dec64 -1278019809081955088,  Dec64 -1273805816360882960,  Dec64 -1269598748856934160,
        Dec64 -1265398583845836816,  Dec64 -1261205298714986512,  Dec64 -1257018870962715920,  Dec64 -1252839278197569552,  Dec64 -1248666498137585936,
        Dec64 -1244500508609584912,  Dec64 -1240341287548460048,  Dec64 -1236188812996477712,  Dec64 -1232043063102582288,  Dec64 -1227904016121704464,
        Dec64 -1223771650414079248,  Dec64 -1219645944444565008,  Dec64 -1215526876781971216,  Dec64 -1211414426098390544,  Dec64 -1207308571168535824,
        Dec64 -1203209290869082896,  Dec64 -1199116564178018320,  Dec64 -1195030370173993744,  Dec64 -1190950688035682064,  Dec64 -1186877497041142800,
        Dec64 -1182810776567190032,  Dec64 -1178750506088764944,  Dec64 -1174696665178315536,  Dec64 -1170649233505179664,  Dec64 -1166608190834972176,
        Dec64 -1162573517028979984,  Dec64 -1158545192043557904,  Dec64 -1154523195929532432,  Dec64 -1150507508831608592,  Dec64 -1146498110987781392,
        Dec64 -1142494982728753680,  Dec64 -1138498104477355280,  Dec64 -1134507456747970064,  Dec64 -1130523020145965072,  Dec64 -1126544775367125264,
        Dec64 -1122572703197093136,  Dec64 -1118606784510809872,  Dec64 -1114647000271965200,  Dec64 -1110693331532447504,  Dec64 -1106745759431800336,
        Dec64 -1102804265196682768,  Dec64 -1098868830140333328,  Dec64 -1094939435662039056,  Dec64 -1091016063246607120,  Dec64 -1087098694463842064,
        Dec64 -1083187310968025616,  Dec64 -1079281894497402640,  Dec64 -1075382426873667600,  Dec64 -1071488890001458704,  Dec64 -1067601265867852048,
        Dec64 -1063719536541864208,  Dec64 -1059843684173953808,  Dec64 -1055973690995529488,  Dec64 -1052109539318462480,  Dec64 -1048251211534599952,
        Dec64 -1044398690115283984,  Dec64 -1040551957610874384,  Dec64 -1036710996650273808,  Dec64 -1032875789940457488,  Dec64 -1029046320266005776,
        Dec64 -1025222570488640528,  Dec64 -1021404523546765328,  Dec64 -1017592162455007760,  Dec64 -1013785470303767312,  Dec64 -1009984430258764560,
        Dec64 -1006189025560594192,  Dec64 -1002399239524282640,  Dec64 -998615055538846736,  Dec64 -994836457066858000,  Dec64 -991063427644008464,
        Dec64 -987295950878680592,  Dec64 -983534010451518992,  Dec64 -979777590115007760,  Dec64 -976026673693048080,  Dec64 -972281245080541456,
        Dec64 -968541288242973968,  Dec64 -964806787216004368,  Dec64 -961077726105056272,  Dec64 -957354089084910864,  Dec64 -953635860399304720,
        Dec64 -949923024360529680,  Dec64 -946215565349035792,  Dec64 -942513467813036560,  Dec64 -938816716268118288,  Dec64 -935125295296851216,
        Dec64 -931439189548402704,  Dec64 -927758383738156304,  Dec64 -924082862647329552,  Dec64 -9204126111225973009,  Dec64 -9167476140757165841,
        Dec64 -9130878564831548689,  Dec64 -9094333233857201937,  Dec64 -9057839998881943569,  Dec64 -9021398711589686801,  Dec64 -8985009224296822801,
        Dec64 -8948671389948630801,  Dec64 -8912385062115711249,  Dec64 -8876150094990445841,  Dec64 -8839966343383482385,  Dec64 -8803833662720244241,
        Dec64 -8767751909037464081,  Dec64 -8731720938979741969,  Dec64 -8695740609796128785,  Dec64 -8659810779336732433,  Dec64 -8623931306049348113,
        Dec64 -8588102048976112401,  Dec64 -8552322867750180625,  Dec64 -8516593622592427025,  Dec64 -8480914174308169233,  Dec64 -8445284384283912977,
        Dec64 -8409704114484122897,  Dec64 -8374173227448012049,  Dec64 -8338691586286356753,  Dec64 -8303259054678332433,  Dec64 -8267875496868369425,
        Dec64 -8232540777663034129,  Dec64 -8197254762427928337,  Dec64 -8162017317084612625,  Dec64 -8126828308107547409,  Dec64 -8091687602521059601,
        Dec64 -8056595067896325905,  Dec64 -8021550572348378641,  Dec64 -7986553984533132305,  Dec64 -7951605173644430097,  Dec64 -7916704009411109393,
        Dec64 -7881850362094089489,  Dec64 -7847044102483476497,  Dec64 -7812285101895691281,  Dec64 -7777573232170612497,  Dec64 -7742908365668742929,
        Dec64 -7708290375268392977,  Dec64 -7673719134362883857,  Dec64 -7639194516857768721,  Dec64 -7604716397168073745,  Dec64 -7570284650215556625,
        Dec64 -7535899151425984785,  Dec64 -7501559776726430481,  Dec64 -7467266402542585361,  Dec64 -7433018905796090897,  Dec64 -7398817163901889553,
        Dec64 -7364661054765591569,  Dec64 -7330550456780858897,  Dec64 -7296485248826808849,  Dec64 -7262465310265432593,  Dec64 -7228490520939032849,
        Dec64 -7194560761167675921,  Dec64 -7160675911746663441,  Dec64 -7126835853944019729,  Dec64 -7093040469497993745,  Dec64 -7059289640614582289,
        Dec64 -7025583249965063185,  Dec64 -6991921180683550993,  Dec64 -6958303316364563729,  Dec64 -6924729541060609041,  Dec64 -6891199739279783697,
        Dec64 -6857713795983390737,  Dec64 -6824271596583570961,  Dec64 -6790873026940950033,  Dec64 -6757517973362300945,  Dec64 -6724206322598222609,
        Dec64 -6690937961840832273,  Dec64 -6657712778721473297,  Dec64 -6624530661308438289,  Dec64 -6591391498104706065,  Dec64 -6558295178045694225,
        Dec64 -6525241590497025297,  Dec64 -6492230625252308241,  Dec64 -6459262172530933777,  Dec64 -6426336122975883281,  Dec64 -6393452367651552785,
        Dec64 -6360610798041590289,  Dec64 -6327811306046747665,  Dec64 -6295053783982744593,  Dec64 -6262338124578148881,  Dec64 -6229664220972267281,
        Dec64 -6197031966713052433,  Dec64 -6164441255755020817,  Dec64 -6131891982457185553,  Dec64 -6099384041581002769,  Dec64 -6066917328288327697,
        Dec64 -6034491738139388433,  Dec64 -6002107167090769169,  Dec64 -5969763511493406481,  Dec64 -5937460668090599953,  Dec64 -5905198534016033809,
        Dec64 -5872977006791811601,  Dec64 -5840795984326503185,  Dec64 -5808655364913203217,  Dec64 -5776555047227603985,  Dec64 -5744494930326076689,
        Dec64 -5712474913643769617,  Dec64 -5680494896992712209,  Dec64 -5648554780559937297,  Dec64 -5616654464905609489,  Dec64 -5584793850961169169,
        Dec64 -5552972840027485969,  Dec64 -5521191333773024017,  Dec64 -5489449234232018961,  Dec64 -5457746443802666513,  Dec64 -5426082865245320977,
        Dec64 -5394458401680706321,  Dec64 -5362872956588137489,  Dec64 -5331326433803752721,  Dec64 -5299818737518757137,  Dec64 -5268349772277676561,
        Dec64 -5236919442976622865,  Dec64 -5205527654861568785,  Dec64 -5174174313526634513,  Dec64 -5142859324912383761,  Dec64 -5111582595304131089,
        Dec64 -5080344031330258961,  Dec64 -5049143539960545809,  Dec64 -5017981028504502545,  Dec64 -4986856404609721105,  Dec64 -4955769576260232721,
        Dec64 -4924720451774875153,  Dec64 -4893708939805669905,  Dec64 -4862734949336210705,  Dec64 -4831798389680060177,  Dec64 -4800899170479157009,
        Dec64 -4770037201702231825,  Dec64 -4739212393643234577,  Dec64 -4708424656919768337,  Dec64 -4677673902471535377,  Dec64 -4646960041558791185,
        Dec64 -4616282985760807953,  Dec64 -4585642646974346769,  Dec64 -4555038937412140049,  Dec64 -4524471769601382417,  Dec64 -4493941056382230033,
        Dec64 -4463446710906310161,  Dec64 -4432988646635238161,  Dec64 -4402566777339144209,  Dec64 -4372181017095208721,  Dec64 -4341831280286206225,
        Dec64 -4311517481599057937,  Dec64 -4281239536023392273,  Dec64 -4250997358850115857,  Dec64 -4220790865669989649,  Dec64 -4190619972372217105,
        Dec64 -4160484595143037457,  Dec64 -4130384650464330513,  Dec64 -4100320055112225553,  Dec64 -4070290726155722769,  Dec64 -4040296580955319825,
        Dec64 -4010337537161646609,  Dec64 -3980413512714110481,  Dec64 -3950524425839545617,  Dec64 -3920670195050873873,  Dec64 -3890850739145770769,
        Dec64 -3861065977205340945,  Dec64 -3831315828592799761,  Dec64 -3801600212952164881,  Dec64 -3771919050206952977,  Dec64 -3742272260558884369,
        Dec64 -3712659764486597649,  Dec64 -3683081482744368401,  Dec64 -3653537336360836881,  Dec64 -3624027246637743889,  Dec64 -3594551135148672017,
        Dec64 -3565108923737795601,  Dec64 -3535700534518636817,  Dec64 -3506325889872829713,  Dec64 -3476984912448891921,  Dec64 -3447677525161000465,
        Dec64 -3418403651187778833,  Dec64 -3389163213971087377,  Dec64 -3359956137214822417,  Dec64 -3330782344883721745,  Dec64 -3301641761202177041,
        Dec64 -3272534310653053201,  Dec64 -3243459917976512785,  Dec64 -3214418508168850961,  Dec64 -3185410006481331985,  Dec64 -3156434338419036433,
        Dec64 -3127491429739712785,  Dec64 -3098581206452635921,  Dec64 -3069703594817473297,  Dec64 -3040858521343154705,  Dec64 -3012045912786750993,
        Dec64 -2983265696152358929,  Dec64 -2954517798689989905,  Dec64 -2925802147894468113,  Dec64 -2897118671504332817,  Dec64 -2868467297500746257,
        Dec64 -2839847954106410769,  Dec64 -2811260569784488209,  Dec64 -2782705073237527569,  Dec64 -2754181393406398481,  Dec64 -2725689459469229329,
        Dec64 -2697229200840353041,  Dec64 -2668800547169257745,  Dec64 -2640403428339542545,  Dec64 -2612037774467881489,  Dec64 -2583703515902990097,
        Dec64 -2555400583224599569,  Dec64 -2527128907242437393,  Dec64 -2498888418995211281,  Dec64 -2470679049749600017,  Dec64 -2442500730999250193,
        Dec64 -2414353394463777809,  Dec64 -2386236972087775249,  Dec64 -2358151396039824657,  Dec64 -2330096598711515409,  Dec64 -2302072512716467985,
        Dec64 -2274079070889362705,  Dec64 -2246116206284973329,  Dec64 -2218183852177208081,  Dec64 -2190281942058152465,  Dec64 -2162410409637119761,
        Dec64 -2134569188839706897,  Dec64 -2106758213806853137,  Dec64 -2078977418893906193,  Dec64 -2051226738669692945,  Dec64 -2023506107915593745,
        Dec64 -1995815461624623377,  Dec64 -1968154735000516113,  Dec64 -1940523863456816401,  Dec64 -1912922782615973649,  Dec64 -1885351428308442129,
        Dec64 -1857809736571786769,  Dec64 -1830297643649791249,  Dec64 -1802815085991574033,  Dec64 -1775362000250706449,  Dec64 -1747938323284337169,
        Dec64 -1720543992152321041,  Dec64 -1693178944116351761,  Dec64 -1665843116639100177,  Dec64 -1638536447383356689,  Dec64 -1611258874211177745,
        Dec64 -1584010335183038993,  Dec64 -1556790768556989713,  Dec64 -1529600112787813905,  Dec64 -1502438306526196497,  Dec64 -1475305288617890577,
        Dec64 -1448200998102893841,  Dec64 -1421125374214624529,  Dec64 -1394078356379105297,  Dec64 -1367059884214150161,  Dec64 -1340069897528555025,
        Dec64 -1313108336321293329,  Dec64 -1286175140780716561,  Dec64 -1259270251283756305,  Dec64 -1232393608395134225,  Dec64 -1205545152866573329,
        Dec64 -1178724825636013841,  Dec64 -1151932567826835473,  Dec64 -1125168320747079185,  Dec64 -1098432025888678417,  Dec64 -1071723624926689809,
        Dec64 -1045043059718531089,  Dec64 -1018390272303220497,  Dec64 -991765204900622097,  Dec64 -965167799910694161,  Dec64 -938597999912740625,
        Dec64 -9120557476646688018,  Dec64 -8855409861022477842,  Dec64 -8590536583383737618,  Dec64 -8325937076623368978,  Dec64 -8061610775390929682,
        Dec64 -7797557116085387538,  Dec64 -7533775536847907858,  Dec64 -7270265477554681618,  Dec64 -7007026379809789202,  Dec64 -6744057686938099474,
        Dec64 -6481358843978207762,  Dec64 -6218929297675408658,  Dec64 -5956768496474703634,  Dec64 -5694875890513847314,  Dec64 -5433250931616427282,
        Dec64 -5171893073284978450,  Dec64 -4910801770694135826,  Dec64 -4649976480683817746,  Dec64 -4389416661752447762,  Dec64 -4129121774050208786,
        Dec64 -3869091279372331538,  Dec64 -3609324641152418322,  Dec64 -3349821324455799826,  Dec64 -3090580795972924434,  Dec64 -2831602524012783890,
        Dec64 -2572885978496368658,  Dec64 -2314430630950159634,  Dec64 -2056235954499650066,  Dec64 -1798301423862901522,  Dec64 -1540626515344131858,
        Dec64 -1283210706827335954,  Dec64 -1026053477769937170,  Dec64 -7691543091964727571,  Dec64 -5125126836923077907,  Dec64 -2561280853973845523,
        Dec64 237,  Dec64 2558720852693845229,  Dec64 5114886816443023597,  Dec64 7668502988284105965,  Dec64 1021957445001588206,
        Dec64 1276810626826003182,  Dec64 1531410349452151022,  Dec64 1785757116524862190,  Dec64 2039851430189279982,  Dec64 2293693791096807662,
        Dec64 2547284698411029486,  Dec64 2800624649813597422,  Dec64 3053714141510093550,  Dec64 3306553668235860206,  Dec64 3559143723261804014,
        Dec64 3811484798400167918,  Dec64 4063577384010278382,  Dec64 4315421969004262382,  Dec64 4567019040852736238,  Dec64 4818369085590467054,
        Dec64 5069472587822006766,  Dec64 5320330030727297262,  Dec64 5570941896067249390,  Dec64 5821308664189294062,  Dec64 6071430814032906990,
        Dec64 6321308823135104494,  Dec64 6570943167635914990,  Dec64 6820334322283821806,  Dec64 7069482760441181678,  Dec64 7318388954089614318,
        Dec64 7567053373835367406,  Dec64 7815476488914655726,  Dec64 8063658767198973934,  Dec64 8311600675200382958,  Dec64 8559302678076771566,
        Dec64 8806765239637093614,  Dec64 9053988822346577134,  Dec64 930097388733191407,  Dec64 954772089438640879,  Dec64 979423030197512943,
        Dec64 1004050256724001519,  Dec64 1028653814600495087,  Dec64 1053233749278084847,  Dec64 1077790106077066223,  Dec64 1102322930187442927,
        Dec64 1126832266669423087,  Dec64 1151318160453917935,  Dec64 1175780656343035119,  Dec64 1200219799010570991,  Dec64 1224635633002499311,
        Dec64 1249028202737459439,  Dec64 1273397552507240431,  Dec64 1297743726477264111,  Dec64 1322066768687064559,  Dec64 1346366723050767087,
        Dec64 1370643633357563631,  Dec64 1394897543272185327,  Dec64 1419128496335375087,  Dec64 1443336535964355823,  Dec64 1467521705453297135,
        Dec64 1491684047973780207,  Dec64 1515823606575259887,  Dec64 1539940424185525743,  Dec64 1564034543611158767,  Dec64 1588106007537987823,
        Dec64 1612154858531544303,  Dec64 1636181139037511919,  Dec64 1660184891382177775,  Dec64 1684166157772879599,  Dec64 1708124980298449391,
        Dec64 1732061400929659375,  Dec64 1755975461519660271,  Dec64 1779867203804422639,  Dec64 1803736669403172591,  Dec64 1827583899818827503,
        Dec64 1851408936438428911,  Dec64 1875211820533572591,  Dec64 1898992593260838895,  Dec64 1922751295662218735,  Dec64 1946487968665539055,
        Dec64 1970202653084885231,  Dec64 1993895389621023727,  Dec64 2017566218861818607,  Dec64 2041215181282651375,  Dec64 2064842317246834671,
        Dec64 2088447667006025711,  Dec64 2112031270700638959,  Dec64 2135593168360253423,  Dec64 2159133399904022767,  Dec64 2182652005141079791,
        Dec64 2206149023770939887,  Dec64 2229624495383904751,  Dec64 2253078459461460719,  Dec64 2276510955376678895,  Dec64 2299922022394610415,
        Dec64 2323311699672682223,  Dec64 2346680026261089263,  Dec64 2370027041103185903,  Dec64 2393352783035875823,  Dec64 2416657290789998575,
        Dec64 2439940602990716655,  Dec64 2463202758157898735,  Dec64 2486443794706502383,  Dec64 2509663750946954991,  Dec64 2532862665085531631,
        Dec64 2556040575224733423,  Dec64 2579197519363662575,  Dec64 2602333535398395887,  Dec64 2625448661122357231,  Dec64 2648542934226687727,
        Dec64 2671616392300615151,  Dec64 2694669072831819503,  Dec64 2717701013206800367,  Dec64 2740712250711239407,  Dec64 2763702822530362607,
        Dec64 2786672765749301487,  Dec64 2809622117353451759,  Dec64 2832550914228829935,  Dec64 2855459193162430191,  Dec64 2878346990842577647,
        Dec64 2901214343859281391,  Dec64 2924061288704585455,  Dec64 2946887861772917231,  Dec64 2969694099361437167,  Dec64 2992480037670383087,
        Dec64 3015245712803416559,  Dec64 3037991160767965423,  Dec64 3060716417475565039,  Dec64 3083421518742199279,  Dec64 3106106500288639471,
        Dec64 3128771397740779759,  Dec64 3151416246629975279,  Dec64 3174041082393374703,  Dec64 3196645940374253295,  Dec64 3219230855822344687,
        Dec64 3241795863894169839,  Dec64 3264340999653366255,  Dec64 3286866298071014383,  Dec64 3309371794025963247,  Dec64 3331857522305154543,
        Dec64 3354323517603944943,  Dec64 3376769814526428143,  Dec64 3399196447585753071,  Dec64 3421603451204444399,  Dec64 3443990859714717935,
        Dec64 3466358707358796271,  Dec64 3488707028289224431,  Dec64 3511035856569181423,  Dec64 3533345226172792047,  Dec64 3555635170985437167,
        Dec64 3577905724804062959,  Dec64 3600156921337487343,  Dec64 3622388794206707439,  Dec64 3644601376945202671,  Dec64 3666794702999240431,
        Dec64 3688968805728175855,  Dec64 3711123718404754927,  Dec64 3733259474215411951,  Dec64 3755376106260569839,  Dec64 3777473647554935535,
        Dec64 3799552131027796207,  Dec64 3821611589523314159,  Dec64 3843652055800819183,  Dec64 3865673562535100911,  Dec64 3887676142316699887,
        Dec64 3909659827652195823,  Dec64 3931624650964496623,  Dec64 3953570644593124847,  Dec64 3975497840794504175,  Dec64 3997406271742243055,
        Dec64 4019295969527417839,  Dec64 4041166966158855663,  Dec64 4063019293563414511,  Dec64 4084852983586263535,  Dec64 4106668067991160815,
        Dec64 4128464578460730607,  Dec64 4150242546596740847,  Dec64 4172002003920375791,  Dec64 4193742981872511215,  Dec64 4215465511813987311,
        Dec64 4237169625025879023,  Dec64 4258855352709766639,  Dec64 4280522725988005615,  Dec64 4302171775903993327,  Dec64 4323802533422437103,
        Dec64 4345415029429619695,  Dec64 4367009294733663215,  Dec64 4388585360064793583,  Dec64 4410143256075601903,  Dec64 4431683013341306607,
        Dec64 4453204662360013551,  Dec64 4474708233552974319,  Dec64 4496193757264845551,  Dec64 4517661263763944175,  Dec64 4539110783242504943,
        Dec64 4560542345816934383,  Dec64 4581955981528063983,  Dec64 4603351720341404399,  Dec64 4624729592147395055,  Dec64 4646089626761656303,
        Dec64 4667431853925238767,  Dec64 4688756303304870383,  Dec64 4710063004493205743,  Dec64 4731351987009071087,  Dec64 4752623280297710319,
        Dec64 4773876913731029231,  Dec64 4795112916607838959,  Dec64 4816331318154098159,  Dec64 4837532147523154159,  Dec64 4858715433795984111,
        Dec64 4879881205981433071,  Dec64 4901029493016453359,  Dec64 4922160323766341615,  Dec64 4943273727024975087,  Dec64 4964369731515047151,
        Dec64 4985448365888301807,  Dec64 5006509658725766895,  Dec64 5027553638537987311,  Dec64 5048580333765255407,  Dec64 5069589772777842671,
        Dec64 5090581983876229103,  Dec64 5111556995291331567,  Dec64 5132514835184731887,  Dec64 5153455531648903919,  Dec64 5174379112707439343,
        Dec64 5195285606315272175,  Dec64 5216175040358903535,  Dec64 5237047442656624623,  Dec64 5257902840958738671,  Dec64 5278741262947783151,
        Dec64 5299562736238749167,  Dec64 5320367288379301615,  Dec64 5341154946849997807,  Dec64 5361925739064505839,  Dec64 5382679692369819631,
        Dec64 5403416834046477551,  Dec64 5424137191308775407,  Dec64 5444840791304981999,  Dec64 5465527661117551855,  Dec64 5486197827763338479,
        Dec64 5506851318193805039,  Dec64 5527488159295236335,  Dec64 5548108377888947951,  Dec64 5568712000731496175,  Dec64 5589299054514885359,
        Dec64 5609869565866776303,  Dec64 5630423561350692079,  Dec64 5650961067466224879,  Dec64 5671482110649239791,  Dec64 5691986717272080111,
        Dec64 5712474913643770095,  Dec64 5732946726010216943,  Dec64 5753402180554413807,  Dec64 5773841303396640239,  Dec64 5794264120594661359,
        Dec64 5814670658143928815,  Dec64 5835060941977777647,  Dec64 5855434997967625711,  Dec64 5875792851923169007,  Dec64 5896134529592579055,
        Dec64 5916460056662697967,  Dec64 5936769458759232495,  Dec64 5957062761446948335,  Dec64 5977339990229864175,  Dec64 5997601170551441647,
        Dec64 6017846327794779375,  Dec64 6038075487282801903,  Dec64 6058288674278451439,  Dec64 6078485913984875247,  Dec64 6098667231545615855,
        Dec64 6118832652044797935,  Dec64 6138982200507315439,  Dec64 6159115901899018479,  Dec64 6179233781126898671,  Dec64 6199335863039273199,
        Dec64 6219422172425970671,  Dec64 6239492734018512623,  Dec64 6259547572490297583,  Dec64 6279586712456782575,  Dec64 6299610178475664623,
        Dec64 6319617995047060719,  Dec64 6339610186613688303,  Dec64 6359586777561044719,  Dec64 6379547792217584367,  Dec64 6399493254854897647,
        Dec64 6419423189687888111,  Dec64 6439337620874947823,  Dec64 6459236572518134255,  Dec64 6479120068663344111,  Dec64 6498988133300488943,
        Dec64 6518840790363667439,  Dec64 6538678063731339759,  Dec64 6558499977226499055,  Dec64 6578306554616843247,  Dec64 6598097819614946031,
        Dec64 6617873795878427631,  Dec64 6637634507010123759,  Dec64 6657379976558255087,  Dec64 6677110228016595439,  Dec64 6696825284824639983,
        Dec64 6716525170367771119,  Dec64 6736209907977426671,  Dec64 6755879520931264239,  Dec64 6775534032453326831,  Dec64 6795173465714207215,
        Dec64 6814797843831212527,  Dec64 6834407189868526831,  Dec64 6854001526837373679,  Dec64 6873580877696178415,  Dec64 6893145265350729967,
        Dec64 6912694712654340847,  Dec64 6932229242408007663,  Dec64 6951748877360571119,  Dec64 6971253640208873967,  Dec64 6990743553597920751,
        Dec64 7010218640121033711,  Dec64 7029678922320012015,  Dec64 7049124422685286383,  Dec64 7068555163656076527,  Dec64 7087971167620545519,
        Dec64 7107372456915955183,  Dec64 7126759053828819951,  Dec64 7146130980595060463,  Dec64 7165488259400157167,  Dec64 7184830912379301871,
        Dec64 7204158961617549807,  Dec64 7223472429149971951,  Dec64 7242771336961804015,  Dec64 7262055706988598255,  Dec64 7281325561116372207,
        Dec64 7300580921181757679,  Dec64 7319821808972149487,  Dec64 7339048246225853423,  Dec64 7358260254632233711,  Dec64 7377457855831858927,
        Dec64 7396641071416649199,  Dec64 7415809922930022127,  Dec64 7434964431867037167,  Dec64 7454104619674540015,  Dec64 7473230507751307759,
        Dec64 7492342117448192239,  Dec64 7511439470068261871,  Dec64 7530522586866945519,  Dec64 7549591489052173551,  Dec64 7568646197784519919,
        Dec64 7587686734177342447,  Dec64 7606713119296923375,  Dec64 7625725374162609647,  Dec64 7644723519746951663,  Dec64 7663707576975842287,
        Dec64 7682677566728655087,  Dec64 7701633509838382319,  Dec64 7720575427091772399,  Dec64 7739503339229465583,  Dec64 7758417266946131439,
        Dec64 7777317230890604271,  Dec64 7796203251666018287,  Dec64 7815075349829941743,  Dec64 7833933545894512367,  Dec64 7852777860326570735,
        Dec64 7871608313547792623,  Dec64 7890424925934823151,  Dec64 7909227717819408111,  Dec64 7928016709488526575,  Dec64 7946791921184521455,
        Dec64 7965553373105230575,  Dec64 7984301085404117231,  Dec64 8003035078190400495,  Dec64 8021755371529183471,  Dec64 8040461985441583343,
        Dec64 8059154939904859119,  Dec64 8077834254852540143,  Dec64 8096499950174553327,  Dec64 8115152045717350639,  Dec64 8133790561284034799,
        Dec64 8152415516634486511,  Dec64 8171026931485489391,  Dec64 8189624825510855663,  Dec64 8208209218341551087,  Dec64 8226780129565818095,
        Dec64 8245337578729301231,  Dec64 8263881585335170031,  Dec64 8282412168844240879,  Dec64 8300929348675101935,  Dec64 8319433144204232431,
        Dec64 8337923574766126831,  Dec64 8356400659653414383,  Dec64 8374864418116980463,  Dec64 8393314869366086895,  Dec64 8411752032568492015,
        Dec64 8430175926850570223,  Dec64 8448586571297430255,  Dec64 8466983984953034479,  Dec64 8485368186820317679,  Dec64 8503739195861303023,
        Dec64 8522097030997221359,  Dec64 8540441711108626927,  Dec64 8558773255035514351,  Dec64 8577091681577434095,  Dec64 8595397009493609711,
        Dec64 8613689257503051247,  Dec64 8631968444284670959,  Dec64 8650234588477397999,  Dec64 8668487708680291823,  Dec64 8686727823452656367,
        Dec64 8704954951314152943,  Dec64 8723169110744913135,  Dec64 8741370320185651439,  Dec64 8759558598037777135,  Dec64 8777733962663505135,
        Dec64 8795896432385969135,  Dec64 8814046025489330159,  Dec64 8832182760218888687,  Dec64 8850306654781194223,  Dec64 8868417727344154095,
        Dec64 8886515996037144559,  Dec64 8904601478951117551,  Dec64 8922674194138710767,  Dec64 8940734159614355695,  Dec64 8958781393354383855,
        Dec64 8976815913297135855,  Dec64 8994837737343068143,  Dec64 9012846883354857967,  Dec64 9030843369157512431,  Dec64 9048827212538471151,
        Dec64 9066798431247714799,  Dec64 9084757042997867503,  Dec64 9102703065464303855,  Dec64 9120636516285251823,  Dec64 9138557413061897199,
        Dec64 9156465773358487535,  Dec64 9174361614702435055,  Dec64 9192244954584419311,  Dec64 9210115810458491119,  Dec64 922797419974217456,
        Dec64 924582013981656304,  Dec64 926365364802643184,  Dec64 928147474168033264,  Dec64 929928343805068784,  Dec64 931707975437391088,
        Dec64 933486370785048048,  Dec64 935263531564505840,  Dec64 937039459488658928,  Dec64 938814156266838768,  Dec64 940587623604824560,
        Dec64 942359863204852720,  Dec64 944130876765626864,  Dec64 945900665982328048,  Dec64 947669232546623216,  Dec64 949436578146676208,
        Dec64 951202704467156720,  Dec64 952967613189250288,  Dec64 954731305990667248,  Dec64 956493784545653744,  Dec64 958255050524998896,
        Dec64 960015105596047088,  Dec64 961773951422705392,  Dec64 963531589665453808,  Dec64 965288021981354224,  Dec64 967043250024060656,
        Dec64 968797275443827696,  Dec64 970550099887519984,  Dec64 972301724998622448,  Dec64 974052152417247728,  Dec64 975801383780146928,
        Dec64 977549420720718576,  Dec64 979296264869017072,  Dec64 981041917851762672,  Dec64 982786381292349168,  Dec64 984529656810855408,
        Dec64 986271746024051184,  Dec64 988012650545409520,  Dec64 989752371985112560,  Dec64 991490911950062832,  Dec64 993228272043891184,
        Dec64 994964453866964976,  Dec64 996699459016399088,  Dec64 998433289086061552,  Dec64 1000165945666585840,  Dec64 1001897430345376752,
        Dec64 1003627744706620912,  Dec64 1005356890331294192,  Dec64 1007084868797171696,  Dec64 1008811681678835440,  Dec64 1010537330547683312,
        Dec64 1012261816971937264,  Dec64 1013985142516652784,  Dec64 1015707308743726320,  Dec64 1017428317211904496,  Dec64 1019148169476792304,
        Dec64 1020866867090861808,  Dec64 1022584411603459824,  Dec64 1024300804560817136,  Dec64 1026016047506056688,  Dec64 1027730141979201776,
        Dec64 1029443089517183472,  Dec64 1031154891653851120,  Dec64 1032865549919977968,  Dec64 1034575065843271152,  Dec64 1036283440948379632,
        Dec64 1037990676756901104,  Dec64 1039696774787391984,  Dec64 1041401736555374320,  Dec64 1043105563573343984,  Dec64 1044808257350779376,
        Dec64 1046509819394148336,  Dec64 1048210251206916848,  Dec64 1049909554289557744,  Dec64 1051607730139556848,  Dec64 1053304780251422448,
        Dec64 1055000706116692720,  Dec64 1056695509223942640,  Dec64 1058389191058793968,  Dec64 1060081753103920624,  Dec64 1061773196839058160,
        Dec64 1063463523741011440,  Dec64 1065152735283660784,  Dec64 1066840832937971952,  Dec64 1068527818172002032,  Dec64 1070213692450907888,
        Dec64 1071898457236953840,  Dec64 1073582113989519088,  Dec64 1075264664165105136,  Dec64 1076946109217343728,  Dec64 1078626450597003760,
        Dec64 1080305689751999984,  Dec64 1081983828127399408,  Dec64 1083660867165428464,  Dec64 1085336808305481712,  Dec64 1087011652984128752,
        Dec64 1088685402635121136,  Dec64 1090358058689399792,  Dec64 1092029622575103216,  Dec64 1093700095717573616,  Dec64 1095369479539365104,
        Dec64 1097037775460251120,  Dec64 1098704984897230064,  Dec64 1100371109264534768,  Dec64 1102036149973637872,  Dec64 1103700108433260272,
        Dec64 1105362986049377008,  Dec64 1107024784225225712,  Dec64 1108685504361312752,  Dec64 1110345147855421168,  Dec64 1112003716102616304,
        Dec64 1113661210495254768,  Dec64 1115317632422989552,  Dec64 1116972983272778736,  Dec64 1118627264428890864,  Dec64 1120280477272913136,
        Dec64 1121932623183757808,  Dec64 1123583703537668848,  Dec64 1125233719708229360,  Dec64 1126882673066368240,  Dec64 1128530564980366832,
        Dec64 1130177396815865840,  Dec64 1131823169935871984,  Dec64 1133467885700765168,  Dec64 1135111545468304880,  Dec64 1136754150593637360,
        Dec64 1138395702429301232,  Dec64 1140036202325235440,  Dec64 1141675651628785392,  Dec64 1143314051684709616,  Dec64 1144951403835185648,
        Dec64 1146587709419818480,  Dec64 1148222969775645424,  Dec64 1149857186237142768,  Dec64 1151490360136233456,  Dec64 1153122492802292720,
        Dec64 1154753585562154992,  Dec64 1156383639740119536,  Dec64 1158012656657958128,  Dec64 1159640637634920688,  Dec64 1161267583987741680,
        Dec64 1162893497030647024,  Dec64 1164518378075359984,  Dec64 1166142228431107568,  Dec64 1167765049404627440,  Dec64 1169386842300173040,
        Dec64 1171007608419521264,  Dec64 1172627349061977840,  Dec64 1174246065524383216,  Dec64 1175863759101120496,  Dec64 1177480431084119536,
        Dec64 1179096082762864624,  Dec64 1180710715424400112,  Dec64 1182324330353336560,  Dec64 1183936928831856880,  Dec64 1185548512139722224,
        Dec64 1187159081554279152,  Dec64 1188768638350463728,  Dec64 1190377183800809968,  Dec64 1191984719175453680,  Dec64 1193591245742139888,
        Dec64 1195196764766228464,  Dec64 1196801277510699760,  Dec64 1198404785236161264,  Dec64 1200007289200852464,  Dec64 1201608790660652272,
        Dec64 1203209290869083376,  Dec64 1204808791077318896,  Dec64 1206407292534189040,  Dec64 1208004796486184944,  Dec64 1209601304177466608,
        Dec64 1211196816849866992,  Dec64 1212791335742899184,  Dec64 1214384862093761520,  Dec64 1215977397137343216,  Dec64 1217568942106230512,
        Dec64 1219159498230711536,  Dec64 1220749066738783728,  Dec64 1222337648856157680,  Dec64 1223925245806264048,  Dec64 1225511858810258416,
        Dec64 1227097489087027184,  Dec64 1228682137853193456,  Dec64 1230265806323122928,  Dec64 1231848495708927984,  Dec64 1233430207220475376,
        Dec64 1235010942065389552,  Dec64 1236590701449060336,  Dec64 1238169486574646512,  Dec64 1239747298643082992,  Dec64 1241324138853084912,
        Dec64 1242900008401154288,  Dec64 1244474908481584368,  Dec64 1246048840286466032,  Dec64 1247621805005692656,  Dec64 1249193803826964976,
        Dec64 1250764837935798000,  Dec64 1252334908515524848,  Dec64 1253904016747303408,  Dec64 1255472163810120176,  Dec64 1257039350880796656,
        Dec64 1258605579133994992,  Dec64 1260170849742221808,  Dec64 1261735163875834864,  Dec64 1263298522703047152,  Dec64 1264860927389933296,
        Dec64 1266422379100434416,  Dec64 1267982878996361968,  Dec64 1269542428237405680,  Dec64 1271101027981135600,  Dec64 1272658679383010032,
        Dec64 1274215383596378352,  Dec64 1275771141772488176,  Dec64 1277325955060488688,  Dec64 1278879824607437040,  Dec64 1280432751558302704,
        Dec64 1281984737055972592,  Dec64 1283535782241257200,  Dec64 1285085888252893168,  Dec64 1286635056227551216,  Dec64 1288183287299839216,
        Dec64 1289730582602307312,  Dec64 1291276943265454064,  Dec64 1292822370417730544,  Dec64 1294366865185544432,  Dec64 1295910428693267184,
        Dec64 1297453062063237104,  Dec64 1298994766415764208,  Dec64 1300535542869136624,  Dec64 1302075392539624176,  Dec64 1303614316541483248,
        Dec64 1305152315986962416,  Dec64 1306689391986306544,  Dec64 1308225545647761904,  Dec64 1309760778077581040,  Dec64 1311295090380027120,
        Dec64 1312828483657379312,  Dec64 1314360959009937392,  Dec64 1315892517536025584,  Dec64 1317423160331998704,  Dec64 1318952888492246256,
        Dec64 1320481703109196528,  Dec64 1322009605273321968,  Dec64 1323536596073144048,  Dec64 1325062676595237104,  Dec64 1326587847924233968,
        Dec64 1328112111142829296,  Dec64 1329635467331785456,  Dec64 1331157917569936368,  Dec64 1332679462934192880,  Dec64 1334200104499545840,
        Dec64 1335719843339072240,  Dec64 1337238680523939312,  Dec64 1338756617123408112,  Dec64 1340273654204839664,  Dec64 1341789792833697520,
        Dec64 1343305034073554416,  Dec64 1344819378986094832,  Dec64 1346332828631120880,  Dec64 1347845384066555888,  Dec64 1349357046348448752,
        Dec64 1350867816530979056,  Dec64 1352377695666461168,  Dec64 1353886684805348848,  Dec64 1355394784996238576,  Dec64 1356901997285875440,
        Dec64 1358408322719156464,  Dec64 1359913762339135472,  Dec64 1361418317187027184,  Dec64 1362921988302211312,  Dec64 1364424776722237680,
        Dec64 1365926683482829296,  Dec64 1367427709617887984,  Dec64 1368927856159497456,  Dec64 1370427124137928176,  Dec64 1371925514581641968,
        Dec64 1373423028517295600,  Dec64 1374919666969744624,  Dec64 1376415430962049264,  Dec64 1377910321515476464,  Dec64 1379404339649505520,
        Dec64 1380897486381832432,  Dec64 1382389762728372720,  Dec64 1383881169703266800,  Dec64 1385371708318883056,  Dec64 1386861379585823472,
        Dec64 1388350184512926192,  Dec64 1389838124107270384,  Dec64 1391325199374180336,  Dec64 1392811411317229296,  Dec64 1394296760938243824,
        Dec64 1395781249237307376,  Dec64 1397264877212764912,  Dec64 1398747645861226480,  Dec64 1400229556177571824,  Dec64 1401710609154952944,
        Dec64 1403190805784800496,  Dec64 1404670147056825072,  Dec64 1406148633959023344,  Dec64 1407626267477680880,  Dec64 1409103048597376496,
        Dec64 1410578978300985840,  Dec64 1412054057569685744,  Dec64 1413528287382958064,  Dec64 1415001668718593008,  Dec64 1416474202552694000,
        Dec64 1417945889859680752,  Dec64 1419416731612293360,  Dec64 1420886728781596656,  Dec64 1422355882336983024,  Dec64 1423824193246177264,
        Dec64 1425291662475239920,  Dec64 1426758290988571120,  Dec64 1428224079748914160,  Dec64 1429689029717360112,  Dec64 1431153141853350640,
        Dec64 1432616417114682352,  Dec64 1434078856457510128,  Dec64 1435540460836351472,  Dec64 1437001231204089584,  Dec64 1438461168511977968,
        Dec64 1439920273709642736,  Dec64 1441378547745087984,  Dec64 1442835991564698096,  Dec64 1444292606113242608,  Dec64 1445748392333878256,
        Dec64 1447203351168154864,  Dec64 1448657483556017136,  Dec64 1450110790435809008,  Dec64 1451563272744277232,  Dec64 1453014931416575216,
        Dec64 1454465767386266096,  Dec64 1455915781585327088,  Dec64 1457364974944152048,  Dec64 1458813348391556592,  Dec64 1460260902854779632,
        Dec64 1461707639259489008,  Dec64 1463153558529783280,  Dec64 1464598661588196592,  Dec64 1466042949355701488,  Dec64 1467486422751712752,
        Dec64 1468929082694090736,  Dec64 1470370930099144688,  Dec64 1471811965881636848,  Dec64 1473252190954785008,  Dec64 1474691606230267376,
        Dec64 1476130212618224368,  Dec64 1477568011027263216,  Dec64 1479005002364461296,  Dec64 1480441187535368944,  Dec64 1481876567444013296,
        Dec64 1483311142992902384,  Dec64 1484744915083026928,  Dec64 1486177884613865200,  Dec64 1487610052483385584,  Dec64 1489041419588050416,
        Dec64 1490471986822819056,  Dec64 1491901755081151472,  Dec64 1493330725255011312,  Dec64 1494758898234869488,  Dec64 1496186274909707504,
        Dec64 1497612856167020272,  Dec64 1499038642892820464,  Dec64 1500463635971640560,  Dec64 1501887836286537200,  Dec64 1503311244719094256,
        Dec64 1504733862149424880,  Dec64 1506155689456176880,  Dec64 1507576727516534512,  Dec64 1508996977206221552,  Dec64 1510416439399505904,
        Dec64 1511835114969201392,  Dec64 1513253004786672112,  Dec64 1514670109721834224,  Dec64 1516086430643161328,  Dec64 1517501968417684976,
        Dec64 1518916723911000304,  Dec64 1520330697987267568,  Dec64 1521743891509216496,  Dec64 1523156305338148080,  Dec64 1524567940333939184,
        Dec64 1525978797355044336,  Dec64 1527388877258500336,  Dec64 1528798180899927536,  Dec64 1530206709133535216,  Dec64 1531614462812122352,
        Dec64 1533021442787082480,  Dec64 1534427649908405744,  Dec64 1535833085024682736,  Dec64 1537237748983106800,  Dec64 1538641642629477616,
        Dec64 1540044766808204528,  Dec64 1541447122362308592,  Dec64 1542848710133426672,  Dec64 1544249530961813488,  Dec64 1545649585686345456,
        Dec64 1547048875144524016,  Dec64 1548447400172476912,  Dec64 1549845161604962800,  Dec64 1551242160275374320,  Dec64 1552638397015739376,
        Dec64 1554033872656726256,  Dec64 1555428588027644912,  Dec64 1556822543956451056,  Dec64 1558215741269748208,  Dec64 1559608180792791280,
        Dec64 1560999863349489648,  Dec64 1562390789762408688,  Dec64 1563780960852774896,  Dec64 1565170377440476912,  Dec64 1566559040344069616,
        Dec64 1567946950380775920,  Dec64 1569334108366490864,  Dec64 1570720515115783664,  Dec64 1572106171441900784,  Dec64 1573491078156769008,
        Dec64 1574875236070998000,  Dec64 1576258645993883120,  Dec64 1577641308733408496,  Dec64 1579023225096250352,  Dec64 1580404395887778544,
        Dec64 1581784821912060400,  Dec64 1583164503971862768,  Dec64 1584543442868656112,  Dec64 1585921639402616048,  Dec64 1587299094372626160,
        Dec64 1588675808576281584,  Dec64 1590051782809891312,  Dec64 1591427017868481008,  Dec64 1592801514545795824,  Dec64 1594175273634302704,
        Dec64 1595548295925194224,  Dec64 1596920582208389616,  Dec64 1598292133272539376,  Dec64 1599662949905026544,  Dec64 1601033032891970288,
        Dec64 1602402383018228208,  Dec64 1603771001067398896,  Dec64 1605138887821825008,  Dec64 1606506044062596080,  Dec64 1607872470569550064,
        Dec64 1609238168121277936,  Dec64 1610603137495124464,  Dec64 1611967379467192048,  Dec64 1613330894812342512,  Dec64 1614693684304200688,
        Dec64 1616055748715156208,  Dec64 1617417088816367088,  Dec64 1618777705377761264,  Dec64 1620137599168039664,  Dec64 1621496770954678768,
        Dec64 1622855221503933936,  Dec64 1624212951580840688,  Dec64 1625569961949218032,  Dec64 1626926253371671024,  Dec64 1628281826609593840,
        Dec64 1629636682423170800,  Dec64 1630990821571380464,  Dec64 1632344244811997680,  Dec64 1633696952901595888,  Dec64 1635048946595549680,
        Dec64 1636400226648037872,  Dec64 1637750793812045296,  Dec64 1639100648839365872,  Dec64 1640449792480604656,  Dec64 1641798225485181168,
        Dec64 1643145948601330928,  Dec64 1644492962576108016,  Dec64 1645839268155388656,  Dec64 1647184866083871984,  Dec64 1648529757105084656,
        Dec64 1649873941961380848,  Dec64 1651217421393946864,  Dec64 1652560196142802160,  Dec64 1653902266946802928,  Dec64 1655243634543643376,
        Dec64 1656584299669858800,  Dec64 1657924263060828400,  Dec64 1659263525450776560,  Dec64 1660602087572776688,  Dec64 1661939950158752240,
        Dec64 1663277113939480048,  Dec64 1664613579644592368,  Dec64 1665949348002579184,  Dec64 1667284419740791280,  Dec64 1668618795585441264,
        Dec64 1669952476261607152,  Dec64 1671285462493234416,  Dec64 1672617755003138032,  Dec64 1673949354513005552,  Dec64 1675280261743397872,
        Dec64 1676610477413753840,  Dec64 1677940002242390768,  Dec64 1679268836946507248,  Dec64 1680596982242186224,  Dec64 1681924438844395760,
        Dec64 1683251207466993392,  Dec64 1684577288822726384,  Dec64 1685902683623235568,  Dec64 1687227392579056368,  Dec64 1688551416399622896,
        Dec64 1689874755793267696,  Dec64 1691197411467226608,  Dec64 1692519384127638768,  Dec64 1693840674479550960,  Dec64 1695161283226918128,
        Dec64 1696481211072606448,  Dec64 1697800458718395632,  Dec64 1699119026864981232,  Dec64 1700436916211975920,  Dec64 1701754127457913328,
        Dec64 1703070661300248816,  Dec64 1704386518435362288,  Dec64 1705701699558560496,  Dec64 1707016205364079344,  Dec64 1708330036545085168,
        Dec64 1709643193793678064,  Dec64 1710955677800894192,  Dec64 1712267489256706032,  Dec64 1713578628850027504,  Dec64 1714889097268713200,
        Dec64 1716198895199562736,  Dec64 1717508023328321776,  Dec64 1718816482339684592,  Dec64 1720124272917295856,  Dec64 1721431395743753712,
        Dec64 1722737851500610544,  Dec64 1724043640868375792,  Dec64 1725348764526518768,  Dec64 1726653223153469680,  Dec64 1727957017426621936,
        Dec64 1729260148022335216,  Dec64 1730562615615935728,  Dec64 1731864420881720560,  Dec64 1733165564492958192,  Dec64 1734466047121890800,
        Dec64 1735765869439736816,  Dec64 1737065032116693232,  Dec64 1738363535821936112,  Dec64 1739661381223624688,  Dec64 1740958568988902128,
        Dec64 1742255099783898352,  Dec64 1743550974273731056,  Dec64 1744846193122508784,  Dec64 1746140756993332464,  Dec64 1747434666548297712,
        Dec64 1748727922448496624,  Dec64 1750020525354019824,  Dec64 1751312475923958512,  Dec64 1752603774816407024,  Dec64 1753894422688463856,
        Dec64 1755184420196233712,  Dec64 1756473767994831088,  Dec64 1757762466738380528,  Dec64 1759050517080018928,  Dec64 1760337919671898096,
        Dec64 1761624675165186800,  Dec64 1762910784210071792,  Dec64 1764196247455760880,  Dec64 1765481065550483952,  Dec64 1766765239141495536,
        Dec64 1768048768875076848,  Dec64 1769331655396537072,  Dec64 1770613899350216432,  Dec64 1771895501379486448,  Dec64 1773176462126753520,
        Dec64 1774456782233460208
    ]


proc log*(d: Dec64): Dec64 =
    ## log10 impl
    const log10 = Dec64(coef: 23025850929940457, exp: -16)
    var e = d.exp.int
    var c = d.coef.int64
    if e == -128 or c <= 0:
        return DEC64_NAN
    let e2 = ((63 - countLeadingZeroBits(c)) * 77) shr 8
    # by scaling coefficient by 10**-e_ bring x to 1
    # as close as possible; remember this scale in e
    c = c shl 8
    while true:
        let x = Dec64(coef: c and not 255'i64, exp: int8(-e2 and 255))
        c = x.int64
        if not (DEC64_ONE < x):
            break
    e += e2
    
    var y = Dec64(c) + Dec64(-256)
    var y_scaled = makeDec64(y.coef, y.exp + 3)
    var y2 = normal floor y_scaled
    var tab_idx = clamp(y2.coef.int64, LOG_TAB_A, LOG_TAB_B)
    var c2 = Dec64(coef: tab_idx, exp: -3)
    var log_c2 = LOG_TAB[tab_idx - LOG_TAB_A]
    y -= c2
    y /= Dec64(c2.int64 + (1000*256))

    var s = makeDec64(y.coef, y.exp) # ???
    var p = s
    if abs(log_c2) < DEC64_ONE:
        s += log_c2
        log_c2 = DEC64_ZERO
    y = -y
    for i in times(30):
        p *= y
        let progress = fda(p, Dec64(coef: i + 1, exp: 0), s)
        if progress == s:
            break
        s = progress
    
    if is_nan s:
        return DEC64_NAN
    s += log_c2
    return fma(Dec64(coef: e, exp: 0), log10, s)


proc pow*(coef, exp: Dec64): Dec64 =
    var coef = coef
    var exp = exp

    if is_zero exp:
        return DEC64_ONE

    # Adjust for negative exponent

    if exp.coef < 0:
        coef = 1 / coef
        exp = -exp
    if is_nan coef:
        return DEC64_NAN
    if is_zero coef:
        return DEC64_ZERO

    # If the exponent is an integer, then use the squaring method.

    if exp.coef > 0 and exp.exp == 0:
        var aux = DEC64_ONE
        var n = exp.coef
        if n <= 1:
            return coef
        while n > 1:
            if (n and 1) != 0:
                aux *= coef
            coef *= coef
            n = n div 2
        if n == 1:
            return aux * coef
        else:
            return aux
    
    # Otherwise do it the hard way.

    return exp(log(coef) * exp)


# The seed variables contain the random number generator's state.
# They can be set by dec64_seed.
var
    seed0 = DEC64_E
    seed1 = DEC64_2PI

proc random*(_: type Dec64): Dec64 =
    # Return a number between 0 and 1 containing 16 randomy digits.
    # It uses xorshift128+.
    while true:
        var s1 = seed0;
        var s0 = seed1
        s1 = s1 xor (s1 shl 23)
        s1 = s1 xor (s0 xor (s0 shr 5) xor (s1 shr 18))
        seed0 = s0
        seed1 = s1
        let mantissa = (s1 + s0) shr 10

        # mantissa contains an integer between 0 and 18014398509481983.
        # If it is less than or equal to 9999999999999999 then we are done.

        if mantissa <= 9999999999999999'i64:
            return makeDec64(mantissa, -16)


proc root*(_: type Dec64, index, radicand: Dec64): Dec64 =
    var index = normal index
    if is_nan(radicand) or
        is_zero(index) or
        index.coef < 0 or
        index.exp != 0 or
        (radicand.coef < 0 and (index.coef and 1) == 0):
            return DEC64_NAN
    if is_zero radicand: return DEC64_ZERO
    if index == DEC64_ONE: return radicand
    if index == DEC64_2: return sqrt radicand
    let index_minus_one = DEC64_NEG_ONE + index
    result = DEC64_ONE
    var prosult = DEC64_NAN
    while true:
        let progress = (
            (result * index_minus_one) +
            (radicand / pow(result, index_minus_one))
        ) / index
        if progress == result: return result
        if progress == prosult: return (progress + result) / DEC64_2
        prosult = result
        result = progress


proc seed*(_: type Dec64, part0, part1: uint64): void =
    # Seed the dec64_random function. It takes any 128 bits as the seed value.
    # The seed must contain at least one 1 bit.
    seed0 = part0
    seed1 = part1
    if (seed0 or seed1) == 0:
        seed1 = 1


proc sin*(radians: Dec64): Dec64 =
    var radians = radians mod DEC64_2PI
    while DEC64_PI < radians:
        radians -= DEC64_PI
        radians -= DEC64_PI
    while radians < DEC64_NPI:
        radians += DEC64_PI
        radians += DEC64_PI
    var neg = false
    if radians.coef < 0:
        radians = -radians
        neg = true
    if DEC64_HALF_PI < radians:
        radians = DEC64_PI - radians
    
    if radians == DEC64_HALF_PI:
        result = DEC64_ONE
    else:
        let x2 = -(radians * radians)
        var term = radians
        result = term
        for order in times(30):
            term *= x2
            term /= Dec64(coef: (order*2)*(order*2 + 1), exp: 0)
            let progress = result + term
            if progress == result:
                break
            result = progress
    
    if neg: result = -result
    return result


proc sqrt*(radicand: Dec64): Dec64 =
    if not(is_nan radicand) and radicand.coef >= 0:
        if radicand.coef == 0: return DEC64_ZERO
        result = radicand
        while true:
            let progress = (result + (radicand / result)) / DEC64_2
            if progress == result:
                return result
            result = progress
        return result
    else:
        return DEC64_NAN


proc tan*(radians: Dec64): Dec64 =
    return sin(radians) / cos(radians)


# this is probably good enough for now lol
proc `$`*(d: Dec64): string =
    if d.coef == 0: return "0"
    let d = normal d

    result = $abs(d.coef)
    
    if d.exp > 0:
        for i in times(d.exp):
            result.add "0"
    elif d.exp < 0:
        if d.exp == -128: return "NaN"
        if d.exp.abs == result.len: result.insert("0", 0)
        elif d.exp.abs > result.len:
            result.insert("0", 0)
            for i in times(1 + d.exp.abs - result.len):
                result.insert("0", 0)
        result.insert(".", result.len + d.exp)

    if d.coef < 0:
        result.insert("-", 0)


proc testAdd(a, b: Dec64): string =
    return $a & " + " & $b & " = " & $(a + b)

proc testSub(a, b: Dec64): string =
    return $a & " - " & $b & " = " & $(a - b)

proc testDiv(a, b: Dec64): string =
    return $a & " / " & $b & " = " & $(a / b)


# testing
if true:
    echo testAdd(makeDec64(1, -1), makeDec64(-2, -1))
    echo testAdd(makeDec64(-1, -1), makeDec64(2, -1))
    echo testAdd(makeDec64(1, -1), makeDec64(2, -1))
    echo testSub(makeDec64(4, -1), makeDec64(1, -1))
    echo testSub(makeDec64(1, -1), makeDec64(4, -1))
    echo testSub(makeDec64(1, -1), makeDec64(1, -1))
    echo testSub(makeDec64(12, -1), makeDec64(23, -1))
    echo testSub(makeDec64(12, -1), makeDec64(34, -1))

    echo "negative " & $makeDec64(54, -1) & " = " & $(-makeDec64(54, -1))
    echo "negative " & $makeDec64(-54, -1) & " = " & $(-makeDec64(-54, -1))
    echo "negative " & $DEC64_ZERO & " = " & $(-DEC64_ZERO)

    echo testDiv(makeDec64(10, 0), makeDec64(2, 0))
    echo testDiv(makeDec64(10, 0), makeDec64(5, 0))
    echo testDiv(makeDec64(1, 0), makeDec64(3, 0))
    echo "(1 / 3) * 3) = " & $((makeDec64(1, 0) / makeDec64(3, 0)) * makeDec64(3, 0))
    echo "(2 / 3) * 3) = " & $((makeDec64(2, 0) / makeDec64(3, 0)) * makeDec64(3, 0))