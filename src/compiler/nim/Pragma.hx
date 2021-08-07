package compiler.nim;

// --multimethods:on
// {.warning[ProveField]: on.}
// callOperator

// https://github.com/zielmicha/collections.nim/blob/master/collections/iface.nim
// https://forum.nim-lang.org/t/3642#22706
// https://github.com/andreaferretti/interfaced

enum abstract Experimental(String) {
	final ImplicitDeref = "implicitDeref";
	final CodeReordering = "codeReordering";
	final DotOperators = "dotOperators";
	final NotNil = "notnil";
	final StrictNotNil = "strictNotNil";
	final CaseStmtMacros = "caseStmtMacros";
}

enum abstract Optimize(String) {
	final None = "none";
	final Speed = "speed";
	final Size = "size";
}

@:using(compiler.nim.Pragma.PragmaTools)
enum Pragma {
	// Common
	PAcyclic;
	PBase;
	PBorrow(?ops: Array<String>);
	PByCopy;
	PByRef;
	PCast(p: Pragma);
	PClosure;
	PCompileTime;
	PComputedGoto;
	PDiscardable;
	PError;
	PExperimental(?e: Experimental);
	PFatal(msg: String);
	PFinal;
	PGlobal;
	PHint(msg: String);
	PInheritable;
	PInline;
	PLine(?info: {file: String, line: Int});
	PLinearScanEnd;
	PNimCall;
	PNoInit;
	PNoInline;
	PNoReturn;
	PNoSideEffect;
	PPop;
	PPure;
	PPush(pragmas: Array<Pragma>);
	PRegisterProc;
	PRequiresInit;
	PSetHint(hint: Type, status: Bool);
	PSetWarning(warning: Type, status: Bool);
	PSize(size: Expr);
	PShallow;
	PSideEffect;
	PUsed;
	PWarning(msg: String);
	//benign
	//merge
	//compilerproc
	//unchecked
	//partial
	//requires
	//ensures
	//procvar
	//delegator
	//dirty
	//define
	//undef
	//unroll
	//this: self
	
	// Checks
	PChecks(status: Bool);
	PBoundChecks(status: Bool);
	POverflowChecks(status: Bool);
	PNilChecks(status: Bool);
	PNanChecks(status: Bool);
	PInfChecks(status: Bool);
	PFloatChecks(status: Bool);
	PAssertions(status: Bool);
	PWarnings(status: Bool);
	PHints(status: Bool);
	POptimization(opt: Optimize);
	PPatterns(status: Bool);
	PCallConv(conv: String);
	//objChecks
	//fieldChecks
	//staticBoundChecks
	//styleChecks
	//rangeChecks
	
	// C/C++
	PAlign(align: Int);
	PBitsize(size: Int);
	PCDecl;
	PCodegenDecl(code: String);
	//completeStruct
	PCompile(file: String, ?args: String);
	PConstructor;
	PDynlib(?lib: String); // also accepts expr
	PEmit(code: String);
	PExtern(name: String);
	PGcSafe;
	PHeader(header: String);
	PImportC(?name: String);
	PImportCPP(name: String);
	//PInjectStmt(expr: Expr);
	PIncompleteStruct;
	PNoDecl;
	PPacked;
	PPassC(flags: String);
	PUnion;
	PVarargs;
	
	// Experimental
	PPackage;
	PNoRewrite;
	PNoAlias;
	PExplain;
	
	// Effects
	PRaises(exns: Array<Expr>);
	PTags(tags: Array<Expr>);
	PEffects;
	
	// Macros
	PGenSym;
	PInject;
	
	// Misc
	PMagic(name: String); // also accepts type name?
	PPragma(name: String, pragmas: Array<Pragma>);
	PTypedPragma;
	PCustom(expr: Expr);
}

inline function onOff(status: Bool) return status ? "on" : "off";

@:publicFields
class PragmaTools {
	static function toNim(self: Pragma) return self._match(
		at(PAcyclic) => "acyclic",
		at(PBase) => "base",
		at(PBorrow(null)) => "borrow",
		at(PBorrow(ops!!)) => "borrow: " + ops.joinMap(", ", o -> '`$o`'),
		at(PByCopy) => "byCopy",
		at(PByRef) => "byRef",
		at(PCast(p)) => "cast(" + p.toNim() + ")",
		at(PClosure) => "closure",
		at(PCompileTime) => "compileTime",
		at(PComputedGoto) => "computedGoto",
		at(PDiscardable) => "discardable",
		at(PError) => "error",
		at(PExperimental(null)) => "experimental",
		at(PExperimental(e!!)) => 'experimental: "$e"',
		at(PFatal(msg)) => 'fatal: "${msg.escape()}"',
		at(PFinal) => "final",
		at(PGlobal) => "global",
		at(PHint(msg)) => 'hint: "${msg.escape()}"',
		at(PInheritable) => "inheritable",
		at(PInline) => "inline",
		at(PLine(null)) => "line",
		at(PLine({file: f, line: l})) => 'line: ("$f", $l)',
		at(PLinearScanEnd) => "linearScanEnd",
		at(PNimCall) => "nimCall",
		at(PNoInit) => "noInit",
		at(PNoInline) => "noInline",
		at(PNoReturn) => "noReturn",
		at(PNoSideEffect) => "noSideEffect",
		at(PPop) => "pop",
		at(PPure) => "pure",
		at(PPush(pragmas)) => "push " + pragmas.joinMap(", ", p -> p.toNim()),
		at(PRegisterProc) => "registerProc",
		at(PRequiresInit) => "requiresInit",
		at(PSetHint(hint, status)) => "hint[" + hint.toNim() + "]: " + onOff(status),
		at(PSetWarning(warning, status)) => "warning[" + warning.toNim() + "]: " + onOff(status),
		at(PSize(size)) => "size: " + size.toNim(),
		at(PShallow) => "shallow",
		at(PSideEffect) => "sideEffect",
		at(PUsed) => "used",
		at(PWarning(msg)) => 'warning: "${msg.escape()}"',
		
		at(PChecks(status)) => "checks: " + onOff(status),
		at(PBoundChecks(status)) => "boundChecks: " + onOff(status),
		at(POverflowChecks(status)) => "overflowChecks: " + onOff(status),
		at(PNilChecks(status)) => "nilChecks: " + onOff(status),
		at(PNanChecks(status)) => "nanChecks: " + onOff(status),
		at(PInfChecks(status)) => "infChecks: " + onOff(status),
		at(PFloatChecks(status)) => "floatChecks: " + onOff(status),
		at(PAssertions(status)) => "assertions: " + onOff(status),
		at(PWarnings(status)) => "warnings: " + onOff(status),
		at(PHints(status)) => "hints: " + onOff(status),
		at(POptimization(opt)) => 'optimization: $opt',
		at(PPatterns(status)) => "patterns: " + onOff(status),
		at(PCallConv(conv)) => 'callConv: $conv',
		
		at(PAlign(align)) => 'align($align)',
		at(PBitsize(size)) => 'bitsize: $size',
		at(PCDecl) => "cDecl",
		at(PCodegenDecl(code)) => 'codegenDecl: "$code"',
		at(PCompile(file, null)) => 'compile: "$file"',
		at(PCompile(file, args!!)) => 'compile("$file", "${args.escape()}")',
		at(PConstructor) => "constructor",
		at(PDynlib(null)) => "dynlib",
		at(PDynlib(lib)) => 'dynlib: "$lib"',
		at(PEmit(code)) => 'emit: """${code.escape()}"""',
		at(PExtern(name)) => 'extern: "$name"',
		at(PGcSafe) => "gcSafe",
		at(PHeader(header)) => 'header: "$header"',
		at(PImportC(null)) => "importC",
		at(PImportC(name!!)) => 'importC: "$name"',
		at(PImportCPP(name)) => 'importCPP: "$name"',
		at(PIncompleteStruct) => "incompleteStruct",
		at(PNoDecl) => "noDecl",
		at(PPacked) => "packed",
		at(PPassC(flags)) => 'passC: "${flags.escape()}"',
		at(PUnion) => "union",
		at(PVarargs) => "varargs",
		
		at(PPackage) => "package",
		at(PNoRewrite) => "noRewrite",
		at(PNoAlias) => "noAlias",
		at(PExplain) => "explain",
		
		at(PRaises(exns)) => "raises: [" + exns.joinMap(", ", e -> e.toNim()) + "]",
		at(PTags(tags)) => "tags: [" + tags.joinMap(", ", t -> t.toNim()) + "]",
		at(PEffects) => "effects",
		
		at(PGenSym) => "genSym",
		at(PInject) => "inject",
		
		at(PMagic(name)) => 'magic: "$name"',
		at(PPragma(name, pragmas)) => 'pragma: $name, ' + pragmas.joinMap(", ", p -> p.toNim()),
		at(PTypedPragma) => "typedPragma",
		at(PCustom(expr)) => expr.toNim()
	);
}