import macros
#import std/strutils

macro public*(tyDecl: untyped): untyped =
    ## Recursively makes type decls and fields public
    for decl in tyDecl[0]:
        var declNode =
            if decl[0].kind == nnkPragmaExpr:
                decl[0]
            else:
                decl

        if declNode[0].kind == nnkIdent:
            declNode[0] = nnkPostfix.newTree(
                newIdentNode("*"),
                declNode[0]
            )
        
        var decl2 = decl[2]
        if decl2.kind in {nnkRefTy, nnkPtrTy}:
            decl2 = decl2[0]

        case decl2.kind
        of nnkObjectTy:
            for node in decl2[2]:
                case node.kind
                of nnkIdentDefs:
                    for i, def in pairs(node):
                        if i == node.len-2: break
                        case def.kind
                        of nnkIdent, nnkAccQuoted:
                            node[i] = nnkPostfix.newTree(
                                newIdentNode("*"),
                                node[i]
                            )
                        else: break
                of nnkRecCase:
                    if node[0][0].kind in {nnkIdent, nnkAccQuoted}:
                        node[0][0] = nnkPostfix.newTree(
                            newIdentNode("*"),
                            node[0][0]
                        )
                    
                    for i in 1..(node.len-1):
                        let branch = node[i]
                        case branch.kind
                        of nnkOfBranch:    
                            let body =
                                if branch.kind == nnkRecList:
                                    branch.last[0]
                                else:
                                    branch.last
                            case body.kind
                            of nnkIdentDefs:
                                for i, def in pairs(body):
                                    if i == body.len-2: break
                                    case def.kind
                                    of nnkIdent, nnkAccQuoted:
                                        body[i] = nnkPostfix.newTree(
                                            newIdentNode("*"),
                                            def
                                        )
                                    else: break
                            of nnkRecList:
                                for node2 in body:
                                    case node2.kind
                                    of nnkIdentDefs:
                                        for i, def in pairs(node2):
                                            if i == node2.len-2: break
                                            case def.kind
                                            of nnkIdent, nnkAccQuoted:
                                                node2[i] = nnkPostfix.newTree(
                                                    newIdentNode("*"),
                                                    def
                                                )
                                            else: break
                                    else: discard
                            else: discard
                        else: discard # TODO
                    #if contains(decl.treeRepr, "World"): echo node.treeRepr
                else: discard
        else: discard
        
    #echo tyDecl[0].astGenRepr
    return tyDecl[0]

const
    NaN32* = 0x7fc00000'f32
    Inf32* = 0x7f800000'f32
    NegInf32* = 0xff800000'f32