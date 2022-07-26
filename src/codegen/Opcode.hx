package codegen;

enum Opcode {
	// Storage and access

	ONewLocal;
	OGetLocal(id: LocalID);
	OSetLocal(id: LocalID);
	OTeeLocal(id: LocalID);

	OGetField(id: MemberID);
	OSetField(id: MemberID);
	OTeeField(id: MemberID);

	OGetStaticField(id: MemberID);
	OSetStaticField(id: MemberID);
	OTeeStaticField(id: MemberID);


	// Stack manip

	ODup;
	ODup2;
	OSwap;
	OPop;


	// Control flow
	
	OIf(then: Opcodes);
	OIfNot(then: Opcodes);
	OIfElse(then: Opcodes, _else: Opcodes);

	ODo(label: LabelID, _do: Opcodes);

	OLoop(label: LabelID, loop: Opcodes);
	OLoopThen(label: LabelID, loop: Opcodes, then: Opcodes);

	OTry(_try: Opcodes, _catch: Opcodes);

	ORet;
	ORetVoid;

	OThrow(info: String);
	ORethrow;

	OBreak(label: LabelID);

	ONext(label: LabelID);


	// Natives

	ONative(native: String);


	// Values

	OInt8(int: hl.UI8, signed: Bool);
	OInt16(int: hl.UI16, signed: Bool);
	OInt32(int: Int, signed: Bool);
	OInt64(int: /*hl.I64*/haxe.Int64, signed: Bool);
	
	OFloat32(int: Int, dec: String, ?exp: Int);
	OFloat64(int: Int, dec: String, ?exp: Int);
	ODec64(int: Int, dec: String, ?exp: Int);

	OChar(char: Char);

	OStr(str: String);

	OTrue;
	OFalse;

	OThis;

	//OFunc(...);


	// Comprehension

	OBlock(body: Opcodes);


	// Operations

	OVCaseID(t: TypeRef, tag: KindTag);
	OTCaseID(t: TypeRef, tag: KindTag);
	OKindID;
	OKindSlot(i: UInt);
	OKindValue;

	OUpcast(t: TypeRef);
	ODowncast(t: TypeRef);
	ONativeCast(t: TypeRef);
	ODynamicCast(t: TypeRef);

	OOfType(t: TypeRef);

	ONewPtr(t: TypeRef);
	OPtrFromAddr(t: TypeRef);


	// Members

	OGetMember(id: MemberID);
	OSetMember(id: MemberID);
	OTeeMember(id: MemberID);

	OGetStaticMember(t: TypeRef, id: MemberID);
	OSetStaticMember(t: TypeRef, id: MemberID);
	OTeeStaticMember(t: TypeRef, id: MemberID);


	// Messaging

	ODefaultInit(t: TypeRef);

	OInitThis_S(_super: TypeRef, id: InitID);
	OInitThis_M(_super: TypeRef, id: InitID, ?ctx: TVarInstCtx);

	// TODO: implement type variable method mappings for the tvar ctx
	OSend_IS(t: TypeRef, id: InitID);
	OSend_IM(t: TypeRef, id: InitID, ?ctx: TVarInstCtx);

	OSend_SS(t: TypeRef, id: MethodID);
	OSend_MS(t: TypeRef, id: MethodID, ?ctx: TVarInstCtx);

	OSend_SI(t: TypeRef, id: MethodID);
	OSendDynamic_SI(t: TypeRef, id: MethodID);

	OSend_MI(t: TypeRef, id: MethodID, ?ctx: TVarInstCtx);
	OSendDynamic_MI(t: TypeRef, id: MethodID, ?ctx: TVarInstCtx);

	OSend_C(t: TypeRef, id: MethodID, ?ctx: TVarInstCtx);
	OSendDynamic_C(t: TypeRef, id: MethodID, ?ctx: TVarInstCtx);

	OSend_BO(t: TypeRef, id: MethodID, ?ctx: TVarInstCtx);
	OSendDynamic_BO(t: TypeRef, id: MethodID, ?ctx: TVarInstCtx);

	OSend_UO(t: TypeRef, id: MethodID);
	OSendDynamic_UO(t: TypeRef, id: MethodID);


	// Creation

	OInitClass(t: TypeRef);
	OInitTKind(t: TypeRef, tag: KindTag);
	OInitVKind(t: TypeRef, tag: KindTag);
	OInitMultiTKind(t: TypeRef, tag: KindTag);
	OInitMultiVKind(t: TypeRef, tag: KindTag);

	
	// Multi kinds
	
	OMultiKindHasTag(tag: KindTag);
	OMultiKindGetTag(tag: KindTag);
	OMultiKindGetSlot(tag: KindTag, slot: UInt);
	//OMultiKindAddTag(tag: KindTag);
	//OMultiKindRemoveTag(tag: KindTag);
}