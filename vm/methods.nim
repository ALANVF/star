import util
import ids
import typeref
import opcode
import typevar

public:
 type
    Method {.inheritable.} = ref object
        id: MethodID
        body: Opcodes

    SingleMethod = ref object of Method
        name: string
    
    MultiMethod = ref object of Method
        typevars: TypeVars
        params: seq[TypeRef]
        name: string
    
    CastMethod = ref object of Method
        typevars: TypeVars
        `type`: TypeRef
    
    BinaryMethod = ref object of Method
        typevars: TypeVars
        param: TypeRef
        name: string
    
    UnaryMethod = ref object of Method
        name: string

public:
 type
    Init {.inheritable.} = ref object
        id: InitID
        body: Opcodes

    SingleInit = ref object of Init
        name: string
    
    MultiInit = ref object of Init
        typevars: TypeVars
        params: seq[TypeRef]
        name: string