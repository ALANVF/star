Short Term:
- Change Dec to use decimal64 instead of dec64 (which was a total let down as the impl is incomplete/bad)
	- https://speleotrove.com/decimal/
- Fix method lookup/dispatch codegen:
	- Every method ID needs to have a type or declaration associated with it to help deal with collisions and virtual methods
		- This should also fix issues with incorrect dispatch
- Implement codegen for closures:
	- Will probably be implemented intrinsically in the VM via an opcode + special value type, though ideally it should be implemented similar to C++ or Java
- Implement correct method dispatch for categories:
	- Needs to support virtual methods
	- Should not be considered type declarations (currently done so as a shortcut)
- Implement proper / more detailed traceback:
	- Currently only includes tracebacks from try/catch statements


Long Term:
- Website
- More documentation
- More examples
- Finish stdlib
- Finish compiler
- Write LLVM backend