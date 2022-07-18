package codegen;

@:publicFields class TVarMappings {
	final staticMembers = new Map<MemberID, Tuple2<TypeID, MemberID>>();
	final instMembers = new Map<MemberID, Tuple2<TypeID, MemberID>>();

	final taggedCases = new Map<KindTag, Tuple2<TypeID, KindTag>>();
	final valueCases = new Map<KindTag, Tuple2<TypeID, KindTag>>();

	final singleInits = new Map<InitID, Tuple2<TypeID, InitID>>();
	final multiInits = new Map<InitID, Tuple2<TypeID, InitID>>();

	final staticSingleMethods = new Map<MethodID, Tuple2<TypeID, MethodID>>();
	final staticMultiMethods = new Map<MethodID, Tuple2<TypeID, MethodID>>();
	final instSingleMethods = new Map<MethodID, Tuple2<TypeID, MethodID>>();
	final instMultiMethods = new Map<MethodID, Tuple2<TypeID, MethodID>>();
	final castMethods = new Map<MethodID, Tuple2<TypeID, MethodID>>();
	final binaryMethods = new Map<MethodID, Tuple2<TypeID, MethodID>>();
	final unaryMethods = new Map<MethodID, Tuple2<TypeID, MethodID>>();

	function new() {}
}