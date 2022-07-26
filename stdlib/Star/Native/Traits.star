module Traits {
	;== Primitives
	
	type T is native[repr: `void`]
	alias IsVoid = T
	
	type T is native[repr: `bool`]
	alias IsBool = T
	
	type T is native[repr: `int` bits: 8 signed: true]
	alias IsInt8 = T
	
	type T is native[repr: `int` bits: 8 signed: false]
	alias IsUInt8 = T
	
	type T is native[repr: `int` bits: 16 signed: true]
	alias IsInt16 = T
	
	type T is native[repr: `int` bits: 16 signed: false]
	alias IsUInt16 = T
	
	type T is native[repr: `int` bits: 32 signed: true]
	alias IsInt32 = T
	
	type T is native[repr: `int` bits: 32 signed: false]
	alias IsUInt32 = T
	
	type T is native[repr: `int` bits: 64 signed: true]
	alias IsInt64 = T
	
	type T is native[repr: `int` bits: 64 signed: false]
	alias IsUInt64 = T
	
	type T is native[repr: `float` bits: 32]
	alias IsFloat32 = T
	
	type T is native[repr: `float` bits: 64]
	alias IsFloat64 = T

	type T is native[repr: `dec64`]
	alias IsDec64 = T
	
	
	;== Pointers
	
	type T is native[repr: `voidptr`]
	alias IsVoidPtr = T
	
	type T if !(T of IsVoid)
	type U is native[repr: `ptr` elem: T]
	alias IsElemPtr = U
	
	type T if !(T of IsVoid)
	type U is native[repr: `ptr` elem: T]
	alias IsElemPtrOf[T] = U
	
	type T if T of IsVoidPtr || T of IsElemPtr
	alias IsPtr = T
	
	alias IsPtrOf[IsVoid] = IsVoidPtr
	
	type T if !(T of IsVoid)
	alias IsPtrOf[T] = IsElemPtrOf[T]
}