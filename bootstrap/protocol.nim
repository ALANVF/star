# Taken from https://forum.nim-lang.org/t/3642#22706 and https://github.com/zielmicha/collections.nim/blob/master/collections/iface.nim

## Real interfaces for Nim ##
## 
## Does not work very well:
## - interface inheritance doesn't work yet
## - specialized generic typeclasses are currently borked in the compiler
## - does not like `Ordinal` as a type param constraint for some reason

import macros
import os
import sequtils
import algorithm
import typeinfo
import fakehti

{.experimental.}

func deSym(n: NimNode): NimNode =
    # Remove all symbols
    result = n
    for x in 0 ..< result.len:
        if result[x].kind == nnkSym:
            result[x] = ident($result[x])
        else:
            result[x] = result[x].deSym

proc `is-a`*(a, b: void) = discard

proc `-`*(a: pointer, b: pointer): int =
    cast[int](a) - cast[int](b)

proc `+`*(a: pointer, b: int): pointer =
    cast[pointer](cast[int](a) + b)

proc `$`*(p: pointer): string = "0x" & $cast[int](p)

proc `$`*(p: ptr): string =
    if p == nil:
        return "nil"
    elif p == cast[ptr](1):
        return "???"
    else:
        return "ptr " & $(p[])


macro structural*(con, inter, fieldDefs): untyped =
    var
        con = deSym con
        inter = deSym inter
        fieldDefs = deSym fieldDefs

    template addGenericParams(place, params) =
        if place.len == 0 and params.len == 0:
            place = newNimNode(nnkEmpty)
        else:
            for node in params:
                place.add(copy node)

    proc insertAll(a: NimNode; pos: int; b: seq[NimNode]) =
        for n in reversed b:
            a.insert(pos, n)
    
    var genParams: NimNode
    var fields: NimNode
    var genNames: seq[NimNode] = @[]
    if fieldDefs.kind == nnkDo:
        genParams = newNimNode(nnkGenericParams)
        var tmp = copy fieldDefs[3]
        tmp.del(0)
        fields = fieldDefs[6]
        for paramGroup in deSym tmp:
            genParams.add paramGroup
            for param in paramGroup[0..^3]:
                genNames.add ident $param
    else:
        genParams = newNimNode(nnkEmpty)
        fields = fieldDefs
    
    let cName = ident $genSym(nskType, "C!")
    
    var genCon, genInter#[, genInterFromCon]#: NimNode
    if genNames.len == 0:
        genCon = con
        genInter = inter
        #genInterFromCon = inter
    else:
        genCon = nnkBracketExpr.newTree(con)
        for n in genNames: genCon.add ident $n

        genInter = nnkBracketExpr.newTree(inter)
        for n in genNames: genInter.add ident $n
        #[
        genInterFromCon = nnkBracketExpr.newTree(inter)
        for name in genNames:
            let arg = quote: `cName`.`name`
            genInterFromCon.add arg]#

    let
        dummySym = genSym(nskVar, "dummy")
        vtableType = newNimNode(nnkTupleTy)
        vtableValRef = newNimNode(nnkPar)
        vtableValPtr = newNimNode(nnkPar)
        conTests = newNimNode(nnkStmtList)
    
    conTests.add nnkMixinStmt.newTree(ident "is-a")

    #for n in genNames:
    #    conTests.add nnkBindStmt.newTree(ident $n)
    
    let v = ident "v"
    for field in fields.children:
        # Build up vtable tuple type and value, as well as concept tests, from the fields:
        let fname = deSym field[0]
        let ftype = deSym field[1][0]
        
        let a_t = quote:
            tuple[offs: int, tmark: `ftype`]

        var a = nnkIdentDefs.newTree(fname, a_t, newEmptyNode())
        vtableType.add(a)
        
        #[let b_e = quote:
            (addr(`dummySym`.`fname`) - addr(`dummySym`[]), `dummySym`.`fname`)]#
        # dumb fix for operators issue
        let b_e = block:
            let ba = quote: addr(`dummySym`.`fname`)
            let bb = nnkDerefExpr.newTree(dummySym)
            let bc = quote: addr(`bb`)
            let bd = infix(ba, "-", bc)
            quote: (`bd`, `dummySym`.`fname`)
        
        var b = nnkExprColonExpr.newTree(fname, b_e)
        vtableValRef.add(b)
        
        let c_e = quote:
            (addr(`dummySym`.`fname`) - addr(`dummySym`), `dummySym`.`fname`)
        
        var c = nnkExprColonExpr.newTree(fname, c_e)
        vtableValPtr.add(c)
        
        let t = quote: `v`.`fname` is `ftype`
        conTests.add(t)
    
    let vName = ident $genSym(nskType, "T!")
    
    let isa = quote "@":
        #@(ident "is-a") is proc (a: @vName, b: @genInter): void
        @(ident "is-a")(@v, type @genInter)
    
    conTests.add isa
    
    let base = ident "~base"
    let typeObj = ident "~typeObj"
    let vtable = ident "~vtable"

    let nodes = newStmtList()

    let vType = nnkTypeOfExpr.newTree(vName)
    
    let node1 = quote:
        type
            `inter`[] = object
                `base`: RootRef
                `typeObj`: ptr TNimType
                `vtable`: ptr `vtableType`
            
            `con`[] #[{.explain.}]# = concept `v`, `vType`
                `conTests`
        
        # Converter for ref object types:
        # (typedesc is explicitly ruled out to prevent infinite loops in the concept tests)
        #converter conToInter[`cName`: `genCon`; `cName2`: not(typedesc) and ref object and `cName`](x: `cName2`): `genInter` =
        converter conToInter[`cName`: ref object and `genCon`](x: `cName`): `genInter` =
            var vt {.global.} = block:
                var `dummySym` = `cName`()
                `vtableValRef`
            let t = cast[ptr TNimType](cast[ptr int](getTypeInfo(x)))
            echo t[]#.rawTypePtr
            `genInter`(
                # Questionable cast, but sizeof and repr suggest refs are glorified pointers anyway:
                `base`: cast[RootRef](x), # Keeps the proxied object alive, provides base address
                `typeObj`: t,
                `vtable`: addr vt
            )
    
    if genNames.len == 0:
        node1[0][0][1] = newEmptyNode()
        node1[0][1][1] = newEmptyNode()
    else:
        node1[1][2].insertAll(0, genParams.toSeq)
        addGenericParams(node1[0][0][1], genParams)
        addGenericParams(node1[0][1][1], genParams)
        echo node1[0][1].repr
    
    proc getVTableMember(): NimNode =
        #cast[ptr typeof(x.`~vtable`[]).field.tmark](cast[pointer](x.`~base`) + x.`~vtable`.field.offs)[]
        # dumb fix for operators issue
        let a = quote "@": x.`~vtable`
        let b = nnkDerefExpr.newTree(a)
        let c = quote "@": cast[pointer](x.`~base`)
        let d = quote "@": x.`~vtable`.field.offs
        let e = infix(c, "+", d)
        let f = quote "@": cast[ptr typeof(@b).field.tmark](@e)
        return nnkDerefExpr.newTree(f)

    let node2 = quote "@":
        # Syntax sugar for referring to target fields through the proxy
        # This must be separated from the first quote due to issues with the backtick splice
        # Uses the vtable's dummy tmarks to cast to the correct field type
        
        template `.`[](x: @genInter, field: untyped): untyped =
            @(getVTableMember())
        
        template `.=`[](x: @genInter, field: untyped, value: untyped): untyped =
            @(getVTableMember()) = value
    
    # remove dumb implicit gensyms
    node2[0][3][1][0] = ident "x"
    node2[0][3][2][0] = ident "field"

    node2[1][3][1][0] = ident "x"
    node2[1][3][2][0] = ident "field"
    
    if genNames.len == 0:
        node2[0][2] = newNimNode(nnkEmpty)
        node2[1][2] = newNimNode(nnkEmpty)
    else:
        node2[0][2].add deSym(genParams).toSeq
        node2[1][2].add deSym(genParams).toSeq

    nodes.add node1
    nodes.add node2

    #echo nodes.astGenRepr

    return nodes