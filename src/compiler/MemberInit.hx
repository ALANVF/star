package compiler;

enum MemberInit {
	MVar(name: String, init: InitCtor);
	MCtor(type: Type, init: InitCtor);
}