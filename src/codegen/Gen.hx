package codegen;

import typing.*;
import codegen.World2;
import Util.detuple;
import haxe.Int64;

@:publicFields
@:build(util.Overload.build())
class Gen {
	final out: sys.io.FileOutput;
	final world: World2;

	function new(out: sys.io.FileOutput, world: World2) {
		this.out = out;
		this.world = world;

		this.out.bigEndian = false;
	}


	function writeBool(b: Bool) {
		out.writeByte(b ? 1 : 0);
	}
	
	function writeString(str: String) {
		out.writeInt32(str.length);
		out.writeString(str);
	}

	// TODO
	function writeInt64(i: Int64) {
		out.writeInt32(i.low);
		out.writeInt32(i.high);
	}

	// TODO
	function writeUInt64(i: Int64) {
		out.writeInt32(i.low);
		out.writeInt32(i.high);
	}

	function writeDec64(d: Dec64) {
		out.writeInt32(d.low);
		out.writeInt32(d.high);
	}

	inline function writeTypeID(id: TypeID) out.writeInt32(id);
	inline function writeTVarID(id: TVarID) out.writeUInt16(id);
	inline function writeMethodID(id: MethodID) out.writeInt32(id);
	inline function writeInitID(id: InitID) out.writeInt32(id);
	inline function writeMemberID(id: MemberID) out.writeUInt16(id);
	inline function writeKindTag(tag: KindTag) out.writeUInt16(tag);

	inline function writeLocalID(id: LocalID) out.writeUInt16(id);
	inline function writeLabelID(id: LabelID) out.writeByte(id);


	function writeWorld() {
		out.writeString("STARVM");
		out.writeByte(0); out.writeByte(1); out.writeByte(0);

		var foundMain = false;
		for(ss => entry in world.singleStaticMethods) {
			if(ss.isMain) {
				detuple(@final [te, me] = entry);

				writeTypeID(te.id);
				writeMethodID(me.id);

				foundMain = true;

				break;
			}
		}

		if(!foundMain) {
			trace("WARNING: no entrypoint found");
			writeTypeID(0);
			writeMethodID(0);
		}
		
		final declsMap = world.typeDecls.map;
		final declsList = world.typeDecls.list;

		final startDefaultTypes = out.tell();

		for(_ in 0...26) writeTypeID(0); // fill ids

		out.writeInt32(declsList.length);
		for(decl in declsList) {
			writeDecl(decl);
		}

		out.flush();
		out.seek(startDefaultTypes, SeekBegin);

		final defaultTypes = new Array<TypeID>(); defaultTypes.resize(26);
		/*
		0 Value
		1 MultiKind
		2 Void
		3 Bool
		4 Int8
		5 Int16
		6 Int32
		7 Int64
		8 UInt8
		9 UInt16
		10 UInt32
		11 UInt64
		12 Float32
		13 Float64
		14 Dec64
		15 Char
		16 Str
		17 Ptr
		18 Iterable1
		19 Iterable2
		20 Iterator1
		21 Iterator2
		22 Func0
		23 Func1
		24 Func2
		25 Func3
		*/

		defaultTypes[0] = declsMap[Pass2.STD_Value];
		defaultTypes[1] = declsMap[Pass2.STD_MultiKind];
		defaultTypes[2] = declsMap[Pass2.STD_Void];
		defaultTypes[3] = declsMap[Pass2.STD_Bool.getTypeDecl()];
		defaultTypes[6] = declsMap[Pass2.STD_Int.getTypeDecl()];
		defaultTypes[14] = declsMap[Pass2.STD_Dec.getTypeDecl()];
		defaultTypes[15] = declsMap[Pass2.STD_Char.getTypeDecl()];
		defaultTypes[16] = declsMap[Pass2.STD_Str.getTypeDecl()];
		defaultTypes[18] = declsMap[Pass2.STD_Iterable1];
		defaultTypes[19] = declsMap[Pass2.STD_Iterable2];
		defaultTypes[20] = declsMap[Pass2.STD_Iterator1];
		defaultTypes[21] = declsMap[Pass2.STD_Iterator2];
		defaultTypes[22] = declsMap[Pass2.STD_Func0];
		defaultTypes[23] = declsMap[Pass2.STD_Func1];
		defaultTypes[24] = declsMap[Pass2.STD_Func2];
		defaultTypes[25] = declsMap[Pass2.STD_Func3];

		for(decl => id in declsMap) {
			decl._match(
				at(c is Class) => {
					final pos = c.native._match(
						at(None) => continue,
						at(Some(k)) => k._match(
							at(NInt8) => 4,
							at(NUInt8, when(id != defaultTypes[15])) => 8,
							at(NInt16) => 5,
							at(NUInt16) => 9,
							at(NUInt32) => 10,
							at(NInt64) => 7,
							at(NUInt64) => 11,
							at(NFloat32) => 12,
							at(NFloat64) => 13,
							at(NPtr(_)) => 17,
							_ => continue
						)
					);

					if(defaultTypes[pos] == 0) {
						defaultTypes[pos] = id;
					}
					
					if(!defaultTypes.contains(0)) {
						break;
					}
				},
				_ => {}
			);
		}

		for(id in defaultTypes) {
			writeTypeID(id);
		}
	}


	overload function writeDecl(decl: TypeDeclEntry) decl._match(
		at(d is CategoryEntry) => writeDecl(d),
		at(d is OpaqueEntry) => writeDecl(d),
		at(d is NewtypeEntry) => writeDecl(d),
		at(d is ModuleEntry) => writeDecl(d),
		at(d is ClassEntry) => writeDecl(d),
		at(d is ProtocolEntry) => writeDecl(d),
		at(d is TaggedKindEntry) => writeDecl(d),
		at(d is ValueKindEntry) => writeDecl(d),
		_ => throw "bad"
	);

	overload function writeDecl(decl: CategoryEntry) {
		out.writeByte(0);

		writeTypeID(decl.id);
		writeString(decl.name);
		write(decl.typevars);
		
		write(decl.staticSingleMethods);
		write(decl.staticMultiMethods);

		write(decl.pathType);
		write(decl.forType);

		write(decl.staticMembers);

		maybeWrite(decl.staticInit);

		write(decl.singleInits);
		write(decl.multiInits);

		write(decl.instSingleMethods);
		write(decl.instMultiMethods);
		write(decl.castMethods);
		write(decl.binaryMethods);
		write(decl.unaryMethods);

		maybeWrite(decl.staticDeinit);
	}

	overload function writeDecl(decl: OpaqueEntry) {
		out.writeByte(1);

		writeTypeID(decl.id);
		writeString(decl.name);
		write(decl.typevars);
		
		write(decl.staticSingleMethods);
		write(decl.staticMultiMethods);

		write(decl.instSingleMethods);
		write(decl.instMultiMethods);
		write(decl.castMethods);
		write(decl.binaryMethods);
		write(decl.unaryMethods);
	}

	overload function writeDecl(decl: NewtypeEntry) {
		out.writeByte(2);

		writeTypeID(decl.id);
		writeString(decl.name);
		write(decl.typevars);
		
		write(decl.staticSingleMethods);
		write(decl.staticMultiMethods);

		write(decl.base);
		writeBool(decl.noInherit);

		write(decl.staticMembers);

		maybeWrite(decl.staticInit);

		write(decl.instSingleMethods);
		write(decl.instMultiMethods);
		write(decl.castMethods);
		write(decl.binaryMethods);
		write(decl.unaryMethods);

		maybeWrite(decl.staticDeinit);
	}

	overload function writeDecl(decl: ModuleEntry) {
		out.writeByte(3);

		writeTypeID(decl.id);
		writeString(decl.name);
		write(decl.typevars);
		
		write(decl.staticSingleMethods);
		write(decl.staticMultiMethods);

		writeParents(decl.parents);

		write(decl.staticMembers);

		maybeWrite(decl.staticInit);

		maybeWrite(decl.staticDeinit);
	}

	overload function writeDecl(decl: ClassEntry) {
		out.writeByte(4);

		writeTypeID(decl.id);
		writeString(decl.name);
		write(decl.typevars);
		
		write(decl.staticSingleMethods);
		write(decl.staticMultiMethods);

		writeParents(decl.parents);
		
		write(decl.staticMembers);
		write(decl.instMembers);

		maybeWrite(decl.staticInit);
		maybeWrite(decl.defaultInit);

		write(decl.singleInits);
		write(decl.multiInits);

		write(decl.instSingleMethods);
		write(decl.instMultiMethods);
		write(decl.castMethods);
		write(decl.binaryMethods);
		write(decl.unaryMethods);

		write(decl.instSingleMethodVTable);
		write(decl.instMultiMethodVTable);
		write(decl.castMethodVTable);
		write(decl.binaryMethodVTable);
		write(decl.unaryMethodVTable);

		maybeWrite(decl.deinit);
		maybeWrite(decl.staticDeinit);
	}

	overload function writeDecl(decl: ProtocolEntry) {
		out.writeByte(5);

		writeTypeID(decl.id);
		writeString(decl.name);
		write(decl.typevars);
		
		write(decl.staticSingleMethods);
		write(decl.staticMultiMethods);

		writeParents(decl.parents);
		//trace([for(k in decl.instMembers.map.keys()) k.name.name]);
		write(decl.staticMembers);
		write(decl.instMembers);

		maybeWrite(decl.staticInit);
		maybeWrite(decl.defaultInit);

		write(decl.singleInits);
		write(decl.multiInits);

		write(decl.instSingleMethods);
		write(decl.instMultiMethods);
		write(decl.castMethods);
		write(decl.binaryMethods);
		write(decl.unaryMethods);

		write(decl.instSingleMethodVTable);
		write(decl.instMultiMethodVTable);
		write(decl.castMethodVTable);
		write(decl.binaryMethodVTable);
		write(decl.unaryMethodVTable);

		maybeWrite(decl.deinit);
		maybeWrite(decl.staticDeinit);
	}

	overload function writeDecl(decl: TaggedKindEntry) {
		out.writeByte(6);

		writeTypeID(decl.id);
		writeString(decl.name);
		write(decl.typevars);
		
		write(decl.staticSingleMethods);
		write(decl.staticMultiMethods);

		writeParents(decl.parents);
		
		writeBool(decl.isFlags);
		
		write(decl.staticMembers);
		write(decl.instMembers);
		
		write(decl.taggedCases);

		maybeWrite(decl.staticInit);
		maybeWrite(decl.defaultInit);

		write(decl.instSingleMethods);
		write(decl.instMultiMethods);
		write(decl.castMethods);
		write(decl.binaryMethods);
		write(decl.unaryMethods);

		write(decl.instSingleMethodVTable);
		write(decl.instMultiMethodVTable);
		write(decl.castMethodVTable);
		write(decl.binaryMethodVTable);
		write(decl.unaryMethodVTable);

		maybeWrite(decl.deinit);
		maybeWrite(decl.staticDeinit);
	}

	overload function writeDecl(decl: ValueKindEntry) {
		out.writeByte(7);

		writeTypeID(decl.id);
		writeString(decl.name);
		write(decl.typevars);
		
		write(decl.staticSingleMethods);
		write(decl.staticMultiMethods);

		writeParents(decl.parents);
		
		writeBool(decl.isFlags);
		decl.base._andOr(base => {
			writeBool(true);
			write(base);
		}, {
			writeBool(false);
		});
		
		write(decl.staticMembers);

		write(decl.valueCases);

		maybeWrite(decl.staticInit);

		write(decl.instSingleMethods);
		write(decl.instMultiMethods);
		write(decl.castMethods);
		write(decl.binaryMethods);
		write(decl.unaryMethods);

		write(decl.instSingleMethodVTable);
		write(decl.instMultiMethodVTable);
		write(decl.castMethodVTable);
		write(decl.binaryMethodVTable);
		write(decl.unaryMethodVTable);

		maybeWrite(decl.deinit);
		maybeWrite(decl.staticDeinit);
	}


	overload function write(typevars: TypeVarEntries) {
		out.writeByte(typevars.list.length);
		if(typevars.list.length == 0) return;
		for(typevar in typevars.list) write(typevar);
	}

	overload function write(typevar: TypeVarEntry) {
		writeTVarID(typevar.id);
		writeString(typevar.name);
		writeParents(typevar.parents);
	}

	function writeParents(parents: Array<TypeRef>) {
		out.writeByte(parents.length);
		if(parents.length == 0) return;
		for(parent in parents) write(parent);
	}


	overload function write(methods: StaticSingleMethods) {
		out.writeUInt16(methods.list.length);
		for(mth in methods.list) write(mth);
	}

	overload function write(methods: StaticMultiMethods) {
		out.writeUInt16(methods.list.length);
		for(mth in methods.list) write(mth);
	}

	overload function write(methods: InstSingleMethods) {
		out.writeUInt16(methods.list.length);
		for(mth in methods.list) write(mth);
	}

	overload function write(methods: InstMultiMethods) {
		out.writeUInt16(methods.list.length);
		for(mth in methods.list) write(mth);
	}

	overload function write(methods: CastMethods) {
		out.writeUInt16(methods.list.length);
		for(mth in methods.list) write(mth);
	}

	overload function write(methods: BinaryMethods) {
		out.writeUInt16(methods.list.length);
		for(mth in methods.list) write(mth);
	}

	overload function write(methods: UnaryMethods) {
		out.writeUInt16(methods.list.length);
		for(mth in methods.list) write(mth);
	}

	overload function write(inits: SingleInits) {
		out.writeUInt16(inits.list.length);
		for(init in inits.list) write(init);
	}

	overload function write(inits: MultiInits) {
		out.writeUInt16(inits.list.length);
		for(init in inits.list) write(init);
	}

	overload function write(members: MemberEntries) {
		out.writeUInt16(members.list.length);
		for(mem in members.list) write(mem);
	}

	overload function write(cases: Entries<KindTag, TaggedCase, TaggedCaseEntry>) {
		out.writeUInt16(cases.list.length);
		for(c in cases.list) write(c);
	}

	overload function write(cases: Entries<KindTag, ValueCase, ValueCaseEntry>) {
		out.writeUInt16(cases.list.length);
		for(c in cases.list) write(c);
	}

	overload function write(vtable: Map<TypeID, InstSingleMethods>) {
		final size = vtable.size();
		out.writeByte(size);
		if(size == 0) return;
		for(id => methods in vtable) {
			writeTypeID(id);
			write(methods);
		}
	}

	overload function write(vtable: Map<TypeID, InstMultiMethods>) {
		final size = vtable.size();
		out.writeByte(size);
		if(size == 0) return;
		for(id => methods in vtable) {
			writeTypeID(id);
			write(methods);
		}
	}

	overload function write(vtable: Map<TypeID, CastMethods>) {
		final size = vtable.size();
		out.writeByte(size);
		if(size == 0) return;
		for(id => methods in vtable) {
			writeTypeID(id);
			write(methods);
		}
	}

	overload function write(vtable: Map<TypeID, BinaryMethods>) {
		final size = vtable.size();
		out.writeByte(size);
		if(size == 0) return;
		for(id => methods in vtable) {
			writeTypeID(id);
			write(methods);
		}
	}

	overload function write(vtable: Map<TypeID, UnaryMethods>) {
		final size = vtable.size();
		out.writeByte(size);
		if(size == 0) return;
		for(id => methods in vtable) {
			writeTypeID(id);
			write(methods);
		}
	}



	overload function write(mth: SingleMethodEntry) {
		writeMethodID(mth.id);
		write(mth.body);
		writeString(mth.name);
	}

	overload function write(mth: MultiMethodEntry) {
		writeMethodID(mth.id);
		write(mth.body);
		write(mth.typevars);
		out.writeByte(mth.params.length);
		for(param in mth.params) write(param);
		writeString(mth.name);
	}

	overload function write(mth: CastMethodEntry) {
		writeMethodID(mth.id);
		write(mth.body);
		write(mth.typevars);
		write(mth.type);
	}

	overload function write(mth: BinaryMethodEntry) {
		writeMethodID(mth.id);
		write(mth.body);
		write(mth.typevars);
		write(mth.param);
		writeString(mth.name);
	}

	overload function write(mth: UnaryMethodEntry) {
		writeMethodID(mth.id);
		write(mth.body);
		writeString(mth.name);
	}

	overload function write(mem: MemberEntry) {
		writeMemberID(mem.id);
		write(mem.type);
	}

	overload function write(tcase: TaggedCaseEntry) {
		writeKindTag(tcase.id);
		writeString(tcase.name);
		tcase.slots._andOr(slots => {
			out.writeByte(slots.length);
			for(slot in slots) write(slot);
		}, {
			out.writeByte(0);
		});
		maybeWrite(tcase.defaultInit);
	}

	overload function write(vcase: ValueCaseEntry) {
		writeKindTag(vcase.id);
		writeString(vcase.name);
		maybeWrite(vcase.valueInit);
	}


	overload function write(typeref: TypeRef) typeref._match(
		at(TDecl(id)) => {
			out.writeByte(0);
			writeTypeID(id);
		},
		at(TInst(id, ctx)) => {
			out.writeByte(1);
			writeTypeID(id);
			write(ctx);
		},
		at(TTypeVar(id)) => {
			out.writeByte(2);
			write(id);
		},
		at(TThis) => {
			out.writeByte(3);
		}
	);

	overload function write(ctx: TypeInstCtx) {
		out.writeByte(ctx.size());
		for(id => t in ctx) {
			writeTVarID(id);
			write(t);
		}
	}

	overload function write(tvar: TVar) tvar._match(
		at(VDecl(id)) => {
			out.writeByte(0);
			writeTVarID(id);
		},
		at(VMethod(id)) => {
			out.writeByte(1);
			writeTVarID(id);
		}
	);


	overload function write(ctx: Null<TVarInstCtx>) {
		ctx._andOr(ctx => {
			out.writeByte(ctx.size());
			for(tv => e in ctx) { detuple(@final [t, mappings] = e);
				write(tv);
				write(t);
				mappings._andOr(mappings => {
					writeBool(true);
					write(mappings.staticMembers);
					write(mappings.instMembers);
					write(mappings.taggedCases);
					write(mappings.valueCases);
					write(mappings.singleInits);
					write(mappings.multiInits);
					write(mappings.staticSingleMethods);
					write(mappings.staticMultiMethods);
					write(mappings.instSingleMethods);
					write(mappings.instMultiMethods);
					write(mappings.castMethods);
					write(mappings.binaryMethods);
					write(mappings.unaryMethods);
				}, {
					writeBool(false);
				});
			}
		}, {
			out.writeByte(0);
		});
	}

	overload function write(mappings: Map<MemberID, Tuple2<TypeID, MemberID>>) {
		final size = mappings.size();
		out.writeByte(size);
		if(size == 0) return;
		for(id => entry in mappings) {
			writeMemberID(id);
			writeTypeID(entry._1);
			writeMemberID(entry._2);
		}
	}

	overload function write(mappings: Map<KindTag, Tuple2<TypeID, KindTag>>) {
		final size = mappings.size();
		out.writeByte(size);
		if(size == 0) return;
		for(tag => entry in mappings) {
			writeKindTag(tag);
			writeTypeID(entry._1);
			writeKindTag(entry._2);
		}
	}

	overload function write(mappings: Map<InitID, Tuple2<TypeID, InitID>>) {
		final size = mappings.size();
		out.writeByte(size);
		if(size == 0) return;
		for(id => entry in mappings) {
			writeInitID(id);
			writeTypeID(entry._1);
			writeInitID(entry._2);
		}
	}

	overload function write(mappings: Map<MethodID, Tuple2<TypeID, MethodID>>) {
		final size = mappings.size();
		out.writeByte(size);
		if(size == 0) return;
		for(id => entry in mappings) {
			writeMethodID(id);
			writeTypeID(entry._1);
			writeMethodID(entry._2);
		}
	}


	function maybeWrite(ops: Null<Opcodes>) {
		ops._andOr(ops => {
			inline write(ops);
		}, {
			out.writeInt32(0);
		});
	}
	
	overload function write(ops: Opcodes) {
		out.writeInt32(ops.length);
		if(ops.length == 0) return;
		for(op in ops) write(op);
	}
	
	overload function write(op: Opcode) {
		final opIndex = op.getIndex();
		out.writeByte(op._match(
			at(Opcode.ONewLocal ... OLoop(_, _)) => opIndex,
			at(Opcode.OLoopThen(_, _, _) ... ONative(_)) => opIndex - 1,
			
			at(OInt8(_, true)) => opIndex - 1,
			at(OInt8(_, false)) => opIndex - 1 + 1,

			at(OInt16(_, true)) => opIndex - 1 + 1,
			at(OInt16(_, false)) => opIndex - 1 + 1 + 1,
			
			at(OInt32(_, true)) => opIndex - 1 + 2,
			at(OInt32(_, false)) => opIndex - 1 + 2 + 1,
			
			at(OInt64(_, true)) => opIndex - 1 + 3,
			at(OInt64(_, false)) => opIndex - 1 + 3 + 1,

			_ => opIndex - 1 + 4
		));

		op._match(
			at(ONewLocal
			| ODup | ODup2 | OSwap | OPop
			| ORet | ORetVoid
			| ORethrow
			| OTrue | OFalse | OThis
			| OKindID | OKindValue) => {},
			
			at(OGetLocal(id) | OSetLocal(id) | OTeeLocal(id)) => {
				writeLocalID(id);
			},

			at(OGetField(id) | OSetField(id) | OTeeField(id)
			| OGetStaticField(id) | OSetStaticField(id) | OTeeStaticField(id)
			| OGetMember(id) | OSetMember(id) | OTeeMember(id)) => {
				writeMemberID(id);
			},

			at(OIf(ops) | OIfNot(ops) | OBlock(ops)) => {
				write(ops);
			},

			at(OIfElse(ops1, ops2) | OTry(ops1, ops2)) => {
				write(ops1);
				write(ops2);
			},

			at(ODo(label, body)) => {
				writeLabelID(label);
				write(body);
			},

			at(OLoop(label, loop)) => {
				writeLabelID(label);
				write(loop);
				out.writeInt32(0);
			},
			at(OLoopThen(label, loop, then)) => {
				writeLabelID(label);
				write(loop);
				write(then);
			},

			at(OThrow(str) | OStr(str)) => {
				writeString(str);
			},

			at(OBreak(label) | ONext(label)) => {
				writeLabelID(label);
			},

			at(ONative(native)) => {
				out.writeUInt16(NativeOpKind.OPS[native]);
			},

			at(OInt8(int, true)) => out.writeInt8(int),
			at(OInt8(int, false)) => out.writeByte(int),

			at(OInt16(int, true)) => out.writeInt16(int),
			at(OInt16(int, false)) => out.writeUInt16(int),

			at(OInt32(int, _)) => out.writeInt32(int),

			at(OInt64(int, true)) => writeInt64(int),
			at(OInt64(int, false)) => writeUInt64(int),

			at(OFloat32(int, dec, exp)) => {
				out.writeFloat((Std.parseFloat('$int.$dec' + exp._andOr(e => 'e$e', "")) : Single)); // BAD
			},
			at(OFloat64(int, dec, exp)) => {
				out.writeDouble(Std.parseFloat('$int.$dec' + exp._andOr(e => 'e$e', ""))); // BAD
			},
			at(ODec64(int, dec, exp)) => {
				writeDec64(Dec64.fromString('$int.$dec' + exp._andOr(e => 'e$e', ""))); // BAD
			},

			at(OChar(char)) => out.writeByte(char),

			at(OVCaseID(t, tag) | OTCaseID(t, tag)
			| OInitTKind(t, tag) | OInitVKind(t, tag)
			| OInitMultiTKind(t, tag) | OInitMultiVKind(t, tag)) => {
				write(t);
				writeKindTag(tag);
			},

			at(OKindSlot(i)) => out.writeByte(i),

			at(OUpcast(t) | ODowncast(t) | ONativeCast(t) | ODynamicCast(t)
			| OOfType(t)
			| ONewPtr(t) | OPtrFromAddr(t)
			| ODefaultInit(t)
			| OInitClass(t)) => write(t),

			at(OGetStaticMember(t, id) | OSetStaticMember(t, id) | OTeeStaticMember(t, id)) => {
				write(t);
				writeMemberID(id);
			},

			at(OInitThis_S(t, init)) => {
				write(t);
				writeInitID(world.getID(init));
				out.writeByte(0);
			},

			at(OInitThis_M(t, init, ctx) | OSend_IM(t, init, ctx)) => {
				write(t);
				writeInitID(world.getID(init));
				write(ctx);
			},

			at(OSend_IS(t, init)) => {
				write(t);
				writeInitID(world.getID(init));
			},

			at(OSend_SS(t, mth)
			| OSend_SI(t, mth) | OSendDynamic_SI(t, mth)
			| OSend_UO(t, mth) | OSendDynamic_UO(t, mth)) => {
				write(t);
				writeMethodID(world.getID(mth));
			},

			at(OSend_MS(t, mth, ctx)
			| OSend_MI(t, mth, ctx) | OSendDynamic_MI(t, mth, ctx)
			| OSend_C(t, mth, ctx) | OSendDynamic_C(t, mth, ctx)
			| OSend_BO(t, mth, ctx) | OSendDynamic_BO(t, mth, ctx)) => {
				write(t);
				writeMethodID(world.getID(mth));
				write(ctx);
			},

			at(OMultiKindHasTag(tag) | OMultiKindGetTag(tag)) => writeKindTag(tag),

			at(OMultiKindGetSlot(tag, slot)) => {
				writeKindTag(tag);
				out.writeByte(slot);
			}
		);
	}
}