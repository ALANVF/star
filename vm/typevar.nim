import util
import ids
import typeref
import std/tables

{.experimental: "notNil".}

public:
 type
    TypeVarDecl = ref object
        id: TypeVarID
        name: string
        parents: seq[TypeRef]
        # ...
    
    TypeVars = Table[TypeVarID, TypeVarDecl]