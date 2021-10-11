module LLVMC is native `llvm-7` { ;@@ TODO: update to llvm 10 or something
	kind ByteOrdering (Int32) {
		has bigEndian    => 0
		has littleEndian => 1
	}

	kind CodeModel (Int32) {
		has default    => 0
		has jitDefault => 1
		has tiny       => 2
		has small      => 3
		has kernel     => 4
		has medium     => 5
		has large      => 6
	}

	kind Opcode (Int32) {
		has ret            => 1
		has br             => 2
		has switch         => 3
		has indirectBr     => 4
		has invoke         => 5

		has unreachable    => 7
		has callBr         => 67

		has fneg           => 66

		has add            => 8
		has fadd           => 9
		has aub            => 10
		has fsub           => 11
		has mul            => 12
		has fmul           => 13
		has udiv           => 14
		has sdiv           => 15
		has fdiv           => 16
		has urem           => 17
		has srem           => 18
		has frem           => 19

		has lhl            => 20
		has lshr           => 21
		has ashr           => 22
		has and            => 23
		has or             => 24
		has xor            => 25

		has alloca         => 26
		has load           => 27
		has store          => 28
		has getElementPtr  => 29

		has trunc          => 30
		has zext           => 31
		has sext           => 32
		has fpToUI         => 33
		has fpToSI         => 34
		has uiToFP         => 35
		has siToFP         => 36
		has fpTrunc        => 37
		has fpExt          => 38
		has ptrToInt       => 39
		has intToPtr       => 40
		has bitCast        => 41
		has addrSpaceCast  => 60

		has icmp           => 42
		has fcmp           => 43
		has phi            => 44
		has call           => 45
		has select         => 46
		has userOp1        => 47
		has userOp2        => 48
		has vaArg          => 49
		has extractElement => 50
		has insertElement  => 51
		has shuffleVector  => 52
		has extractValue   => 53
		has insertValue    => 54

		has fence          => 55
		has atomicCmpXchg  => 56
		has atomicRMW      => 57

		has resume         => 58
		has landingPad     => 59
		has cleanupRet     => 61
		has catchRet       => 62
		has catchPad       => 63
		has cleanupPad     => 64
		has catchSwitch    => 65
	}

	kind ValueKind (Int32) {
		has argument              => 0
		has basicBlock            => 1
		has memoryUse             => 2
		has memoryDef             => 3
		has memoryPhi             => 4
		has function              => 5
		has globalAlias           => 6
		has globalIFunc           => 7
		has globalVariable        => 8
		has blockAddress          => 9
		has constantExpr          => 10
		has constantArray         => 11
		has constantStruct        => 12
		has constantVector        => 13
		has undefValue            => 14
		has constantAggregateZero => 15
		has constantDataArray     => 16
		has constantDataVector    => 17
		has constantInt           => 18
		has constantFP            => 19
		has constantPointerNull   => 20
		has constantTokenNone     => 21
		has metadataAsValue       => 22
		has inlineAsm             => 23
		has instruction           => 24
	}

	kind TypeKind (Int32) {
		has void      => 0
		has half      => 1
		has float     => 2
		has double    => 3
		has x86_FP80  => 4
		has fp128     => 5
		has ppc_FP128 => 6
		has label     => 7
		has integer   => 8
		has function  => 9
		has struct    => 10
		has array     => 11
		has pointer   => 12
		has vector    => 13
		has metadata  => 14
		has x86_MMX   => 15
		has token     => 16
	}

	kind ModuleFlagBehavior (Int32) {
		has error        => 0
		has warning      => 1
		has require      => 2
		has override     => 3
		has append       => 4
		has appendUnique => 5
	}

	kind InlineAsmDialect (Int32) {
		has att   => 0
		has intel => 1
	}

	kind DiagnosticSeverity (Int32) {
		has error   => 0
		has warning => 1
		has remark  => 2
		has note    => 3
	}

	kind IntPredicate (Int32) {
		has eq  => 32
		has ne  => 33
		has ugt => 34
		has uge => 35
		has ult => 36
		has ule => 37
		has sgt => 38
		has sge => 39
		has slt => 40
		has sle => 41
	}

	kind RealPredicate (Int32) {
		has predicateFalse => 0
		has oeq            => 1
		has ogt            => 2
		has oge            => 3
		has olt            => 4
		has ole            => 5
		has one            => 6
		has ord            => 7
		has uno            => 8
		has ueq            => 9
		has ugt            => 10
		has uge            => 11
		has ult            => 12
		has ule            => 13
		has une            => 14
		has predicateTrue  => 15
	}

	kind Linkage (Int32) {
		has external            => 0
		has availableExternally => 1
		has linkOnceAny         => 2
		has linkOnceODR         => 3
		has linkOnceODRAutoHide => 4
		has weakAny             => 5
		has weakODR             => 6
		has appending           => 7
		has internal            => 8
		has private             => 9
		has dllImport           => 10
		has dllExport           => 11
		has externalWeak        => 12
		has ghost               => 13
		has common              => 14
		has linkerPrivate       => 15
		has linkerPrivateWeak   => 16
	}

	kind Visibility (Int32) {
		has default   => 0
		has hidden    => 1
		has protected => 2
	}

	kind UnnamedAddr (Int32) {
		has none   => 0
		has local  => 1
		has global => 2
	}

	kind DLLStorageClass (Int32) {
		has default => 0
		has import  => 1
		has export  => 2
	}

	kind ThreadLocalMode (Int32) {
		has not            => 0
		has generalDynamic => 1
		has localDynamic   => 2
		has initialExec    => 3
		has localExec      => 4
	}

	kind CallConv (Int32) {
		has c             => 0
		has fast          => 8
		has cold          => 9
		has ghc           => 10
		has hiPE          => 11
		has webKitJS      => 12
		has anyReg        => 13
		has preserveMost  => 14
		has preserveAll   => 15
		has swift         => 16
		has cxxFASTTLS    => 17
		has x86Stdcall    => 64
		has x86Fastcall   => 65
		has armAPCS       => 66
		has armAAPCS      => 67
		has armAAPCSVFP   => 68
		has msp430INTR    => 69
		has x86ThisCall   => 70
		has ptxKernel     => 71
		has ptxDevice     => 72
		has spirFUNC      => 75
		has spirKERNEL    => 76
		has intelOCLBI    => 77
		has x8664SysV     => 78
		has win64         => 79
		has x86VectorCall => 80
		has hhvm          => 81
		has hhvmC         => 82
		has x86INTR       => 83
		has avrINTR       => 84
		has avrSIGNAL     => 85
		has avrBUILTIN    => 86
		has amdGPUVS      => 87
		has amdGPUGS      => 88
		has amdGPUPS      => 89
		has amdGPUCS      => 90
		has amdGPUKERNEL  => 91
		has x86RegCall    => 92
		has amdGPUHS      => 93
		has msp430BUILTIN => 94
		has amdGPULS      => 95
		has amdGPUES      => 96
	}

	kind LandingPadClauseTy (Int32) {
		has catch  => 0
		has filter => 1
	}

	kind AtomicOrdering (Int32) {
		has notAtomic              => 0
		has unordered              => 1
		has monotonic              => 2
		has acquire                => 4
		has release                => 5
		has acquireRelease         => 6
		has sequentiallyConsistent => 7
	}

	kind AtomicRMWBinOp (Int32) {
		has xchg => 0
		has add  => 1
		has sub  => 2
		has and  => 3
		has nand => 4
		has or   => 5
		has xor  => 6
		has max  => 7
		has min  => 8
		has umax => 9
		has umin => 10
	}

	kind VerifierFailureAction (Int32) {
		has abortProcess => 0
		has printMessage => 1
		has returnStatus => 2
	}

	kind BinaryType (Int32) {
		has archive              => 0
		has machOUniversalBinary => 1
		has coffImportFile       => 2
		has ir                   => 3
		has winRes               => 4
		has coff                 => 5
		has elf32L               => 6
		has elf32B               => 7
		has elf64L               => 8
		has elf64B               => 9
		has machO32L             => 10
		has machO32B             => 11
		has machO64L             => 12
		has machO64B             => 13
		has wasm                 => 14
	}

	kind RemarkType (Int32) {
		has unknown           => 0
		has passed            => 1
		has missed            => 2
		has analysis          => 3
		has analysisFPCommute => 4
		has analysisAliasing  => 5
		has failure           => 6
	}
	
	kind DisasmOptions (Int64) is flags {
		has useMarkup
		has printImmHex
		has asmPrinterVariant
		has setInstrComments
		has printLatency
	}
	
	alias AttributeRef
	alias ContextRef
	alias ModuleRef
	alias PassRegistryRef
	alias PassManagerRef
	alias PassManagerBuilderRef
	alias MetadataRef
	alias ValueRef
	alias TypeRef
	alias NamedMDNodeRef
	alias DiagnosticInfoRef
	alias UseRef
	alias BasicBlockRef
	alias BuilderRef
	alias MemoryBufferRef
	alias ModuleProviderRef
	alias GenericValueRef
	alias ExecutionEngineRef
	alias MCJITMemoryManagerRef
	alias TargetDataRef
	alias JITEventListenerRef
	alias SectionIteratorRef
	alias SymbolIteratorRef
	alias RelocationIteratorRef
	alias ObjectFileRef
	alias BinaryRef
	alias RemarkStringRef
	alias RemarkDebugLocRef
	alias RemarkArgRef
	alias RemarkEntryRef
	alias RemarkParserRef
	alias TargetLibraryInfoRef

	alias ModuleFlagEntry
	alias ValueMetadataEntry

	class MCJITCompilerOptions {
		my optLevel (Size_t)
		my modeModel (CodeModel)
		my noFramePointerElim (Bool)
		my enableFastISel (Bool)
	}

	alias DiagnosticHandler = Func[Void, DiagnosticInfoRef, Ptr[Void]]
	alias YieldCallback = Func[Void, ContextRef, Ptr[Void]]
	alias OpInfoCallback = Func[Bool, Ptr[Void], UInt64, UInt64, UInt64, Int32, Ptr[Void]]
	alias SymbolLookupCallback = Func[Str, Ptr[Void], UInt64, Ptr[UInt64], UInt84, Ptr[Str]]
	alias MemoryManagerFinalizeMemoryCallback = Func[Bool, Ptr[Void], Ptr[Str]]
	alias MemoryManagerDestroyCallback = Func[Void, Ptr[Void]]
	alias MemoryManagerAllocateCodeSectionCallback = Func[Array[UInt8], UInt64, UInt32, UInt32, Str]
	alias MemoryManagerAllocateDataSectionCallback = Func[Array[UInt8], UInt64, UInt32, UInt32, Str, Bool]

	;-- Analysis 
	on [verifyModule: (ModuleRef), (VerifierFailureAction), (Ptr[Str])] (Bool) is native `LLVMVerifyModule`
	on [verifyFunction: (ValueRef), (VerifierFailureAction)] (Bool) is native `LLVMVerifyFunction`
	on [viewFunctionCFG: (ValueRef)] is native `LLVMViewFunctionCFG`
	on [viewFunctionCFGOnly: (ValueRef)] is native `LLVMViewFunctionCFGOnly`

	;-- Bit Reader 
	on [parseBitcode: (MemoryBufferRef), (Ptr[ModuleRef]), (Ptr[Str])] (Bool) is native `LLVMParseBitcode`
	on [parseBitcode2: (MemoryBufferRef), (Ptr[ModuleRef])] (Bool) is native `LLVMParseBitcode2`
	on [parseBitcodeInContext: (ContextRef), (MemoryBufferRef), (Ptr[ModuleRef]), (Ptr[Str])] (Bool) is native `LLVMParseBitcodeInContext`
	on [parseBitcodeInContext2: (ContextRef), (MemoryBufferRef), (Ptr[ModuleRef])] (Bool) is native `LLVMParseBitcodeInContext2`
	on [getBitcodeModuleInContext: (ContextRef), (MemoryBufferRef), (Ptr[ModuleRef]), (Ptr[Str])] (Bool) is native `LLVMGetBitcodeModuleInContext`
	on [getBitcodeModuleInContext2: (ContextRef), (MemoryBufferRef), (Ptr[ModuleRef])] (Bool) is native `LLVMGetBitcodeModuleInContext2`
	on [getBitcodeModule: (MemoryBufferRef), (Ptr[ModuleRef]), (Ptr[Str])] (Bool) is native `LLVMGetBitcodeModule`
	on [getBitcodeModule2: (MemoryBufferRef), (Ptr[ModuleRef])] (Bool) is native `LLVMGetBitcodeModule2`

	;-- Bit Writer 
	on [writeBitcodeToFile: (ModuleRef), (Str)] (Bool) is native `LLVMWriteBitcodeToFile`
	on [writeBitcodeToFD: (ModuleRef), (Int32), (Bool), (Bool)] (Bool) is native `LLVMWriteBitcodeToFD`
	on [writeBitcodeToFileHandle: (ModuleRef), (Int32)] (Bool) is native `LLVMWriteBitcodeToFileHandle`
	on [writeBitcodeToMemoryBuffer: (ModuleRef)] (MemoryBufferRef) is native `LLVMWriteBitcodeToMemoryBuffer`

	;-- Transforms
	on [addAggressiveInstCombinerPass: (PassManagerRef)] is native `LLVMAddAggressiveInstCombinerPass`
	
	on [addCoroEarlyPass: (PassManagerRef)] is native `LLVMAddCoroEarlyPass`
	on [addCoroSplitPass: (PassManagerRef)] is native `LLVMAddCoroSplitPass`
	on [addCoroElidePass: (PassManagerRef)] is native `LLVMAddCoroElidePass`
	on [addCoroCleanupPass: (PassManagerRef)] is native `LLVMAddCoroCleanupPass`
	
	on [addArgumentPromotionPass: (PassManagerRef)] is native `LLVMAddArgumentPromotionPass`
	on [addConstantMergePass: (PassManagerRef)] is native `LLVMAddConstantMergePass`
	on [addCalledValuePropagationPass: (PassManagerRef)] is native `LLVMAddCalledValuePropagationPass`
	on [addDeadArgEliminationPass: (PassManagerRef)] is native `LLVMAddDeadArgEliminationPass`
	on [addFunctionAttrsPass: (PassManagerRef)] is native `LLVMAddFunctionAttrsPass`
	on [addFunctionInliningPass: (PassManagerRef)] is native `LLVMAddFunctionInliningPass`
	on [addAlwaysInlinerPass: (PassManagerRef)] is native `LLVMAddAlwaysInlinerPass`
	on [addGlobalDCEPass: (PassManagerRef)] is native `LLVMAddGlobalDCEPass`
	on [addGlobalOptimizerPass: (PassManagerRef)] is native `LLVMAddGlobalOptimizerPass`
	on [addIPConstantPropagationPass: (PassManagerRef)] is native `LLVMAddIPConstantPropagationPass`
	on [addPruneEHPass: (PassManagerRef)] is native `LLVMAddPruneEHPass`
	on [addIPSCCPPass: (PassManagerRef)] is native `LLVMAddIPSCCPPass`
	on [addInternalizePass: (PassManagerRef), (Size_t)] is native `LLVMAddInternalizePass`
	on [addInternalizePassWithMustPreservePredicate: (PassManagerRef), (Ptr[Void]), (Func[Bool, ValueRef, Ptr[Void]])] is native `LLVMAddInternalizePassWithMustPreservePredicate`
	on [addStripDeadPrototypesPass: (PassManagerRef)] is native `LLVMAddStripDeadPrototypesPass`
	on [addStripSymbolsPass: (PassManagerRef)] is native `LLVMAddStripSymbolsPass`
	
	on [passManagerBuilderCreate] (PassManagerBuilderRef) is native `LLVMPassManagerBuilderCreate`
	on [passManagerBuilderDispose: (PassManagerBuilderRef)] is native `LLVMPassManagerBuilderDispose`
	on [passManagerBuilderSetOptLevel: (PassManagerBuilderRef), (Size_t)] is native `LLVMPassManagerBuilderSetOptLevel`
	on [passManagerBuilderSetSizeLevel: (PassManagerBuilderRef), (Size_t)] is native `LLVMPassManagerBuilderSetSizeLevel`
	on [passManagerBuilderSetDisableUnitAtATime: (PassManagerBuilderRef), (Bool)] is native `LLVMPassManagerBuilderSetDisableUnitAtATime`
	on [passManagerBuilderSetDisableUnrollLoops: (PassManagerBuilderRef), (Bool)] is native `LLVMPassManagerBuilderSetDisableUnrollLoops`
	on [passManagerBuilderSetDisableSimplifyLibCalls: (PassManagerBuilderRef), (Bool)] is native `LLVMPassManagerBuilderSetDisableSimplifyLibCalls`
	on [passManagerBuilderUseInlinerWithThreshold: (PassManagerBuilderRef), (Size_t)] is native `LLVMPassManagerBuilderUseInlinerWithThreshold`
	on [passManagerBuilderPopulateFunctionPassManager: (PassManagerBuilderRef), (PassManagerRef)] is native `LLVMPassManagerBuilderPopulateFunctionPassManager`
	on [passManagerBuilderPopulateModulePassManager: (PassManagerBuilderRef), (PassManagerRef)] is native `LLVMPassManagerBuilderPopulateModulePassManager`
	on [passManagerBuilderPopulateLTOPassManager: (PassManagerBuilderRef), (PassManagerRef), (Bool), (Bool)] is native `LLVMPassManagerBuilderPopulateLTOPassManager`
	
	
	on [addAggressiveDCEPass: (PassManagerRef)] is native `LLVMAddAggressiveDCEPass`
	on [addBitTrackingDCEPass: (PassManagerRef)] is native `LLVMAddBitTrackingDCEPass`
	on [addAlignmentFromAssumptionsPass: (PassManagerRef)] is native `LLVMAddAlignmentFromAssumptionsPass`
	on [addCFGSimplificationPass: (PassManagerRef)] is native `LLVMAddCFGSimplificationPass`
	on [addDeadStoreEliminationPass: (PassManagerRef)] is native `LLVMAddDeadStoreEliminationPass`
	on [addScalarizerPass: (PassManagerRef)] is native `LLVMAddScalarizerPass`
	on [addMergedLoadStoreMotionPass: (PassManagerRef)] is native `LLVMAddMergedLoadStoreMotionPass`
	on [addGVNPass: (PassManagerRef)] is native `LLVMAddGVNPass`
	on [addNewGVNPass: (PassManagerRef)] is native `LLVMAddNewGVNPass`
	on [addIndVarSimplifyPass: (PassManagerRef)] is native `LLVMAddIndVarSimplifyPass`
	on [addInstructionCombiningPass: (PassManagerRef)] is native `LLVMAddInstructionCombiningPass`
	on [addJumpThreadingPass: (PassManagerRef)] is native `LLVMAddJumpThreadingPass`
	on [addLICMPass: (PassManagerRef)] is native `LLVMAddLICMPass`
	on [addLoopDeletionPass: (PassManagerRef)] is native `LLVMAddLoopDeletionPass`
	on [addLoopIdiomPass: (PassManagerRef)] is native `LLVMAddLoopIdiomPass`
	on [addLoopRotatePass: (PassManagerRef)] is native `LLVMAddLoopRotatePass`
	on [addLoopRerollPass: (PassManagerRef)] is native `LLVMAddLoopRerollPass`
	on [addLoopUnrollPass: (PassManagerRef)] is native `LLVMAddLoopUnrollPass`
	on [addLoopUnrollAndJamPass: (PassManagerRef)] is native `LLVMAddLoopUnrollAndJamPass`
	on [addLoopUnswitchPass: (PassManagerRef)] is native `LLVMAddLoopUnswitchPass`
	on [addLowerAtomicPass: (PassManagerRef)] is native `LLVMAddLowerAtomicPass`
	on [addMemCpyOptPass: (PassManagerRef)] is native `LLVMAddMemCpyOptPass`
	on [addPartiallyInlineLibCallsPass: (PassManagerRef)] is native `LLVMAddPartiallyInlineLibCallsPass`
	on [addReassociatePass: (PassManagerRef)] is native `LLVMAddReassociatePass`
	on [addSCCPPass: (PassManagerRef)] is native `LLVMAddSCCPPass`
	on [addScalarReplAggregatesPass: (PassManagerRef)] is native `LLVMAddScalarReplAggregatesPass`
	on [addScalarReplAggregatesPassSSA: (PassManagerRef)] is native `LLVMAddScalarReplAggregatesPassSSA`
	on [addScalarReplAggregatesPassWithThreshold: (PassManagerRef), (Int32)] is native `LLVMAddScalarReplAggregatesPassWithThreshold`
	on [addSimplifyLibCallsPass: (PassManagerRef)] is native `LLVMAddSimplifyLibCallsPass`
	on [addTailCallEliminationPass: (PassManagerRef)] is native `LLVMAddTailCallEliminationPass`
	on [addConstantPropagationPass: (PassManagerRef)] is native `LLVMAddConstantPropagationPass`
	on [addDemoteMemoryToRegisterPass: (PassManagerRef)] is native `LLVMAddDemoteMemoryToRegisterPass`
	on [addVerifierPass: (PassManagerRef)] is native `LLVMAddVerifierPass`
	on [addCorrelatedValuePropagationPass: (PassManagerRef)] is native `LLVMAddCorrelatedValuePropagationPass`
	on [addEarlyCSEPass: (PassManagerRef)] is native `LLVMAddEarlyCSEPass`
	on [addEarlyCSEMemSSAPass: (PassManagerRef)] is native `LLVMAddEarlyCSEMemSSAPass`
	on [addLowerExpectIntrinsicPass: (PassManagerRef)] is native `LLVMAddLowerExpectIntrinsicPass`
	on [addTypeBasedAliasAnalysisPass: (PassManagerRef)] is native `LLVMAddTypeBasedAliasAnalysisPass`
	on [addScopedNoAliasAAPass: (PassManagerRef)] is native `LLVMAddScopedNoAliasAAPass`
	on [addBasicAliasAnalysisPass: (PassManagerRef)] is native `LLVMAddBasicAliasAnalysisPass`
	on [addUnifyFunctionExitNodesPass: (PassManagerRef)] is native `LLVMAddUnifyFunctionExitNodesPass`

	on [addLowerSwitchPass: (PassManagerRef)] is native `LLVMAddLowerSwitchPass`
	on [addPromoteMemoryToRegisterPass: (PassManagerRef)] is native `LLVMAddPromoteMemoryToRegisterPass`
	on [addAddDiscriminatorsPass: (PassManagerRef)] is native `LLVMAddAddDiscriminatorsPass`

	on [addLoopVectorizePass: (PassManagerRef)] is native `LLVMAddLoopVectorizePass`
	on [addSLPVectorizePass: (PassManagerRef)] is native `LLVMAddSLPVectorizePass`
	
	;-- Core
	on [initializeCore: (ContextRef)] is native `LLVMInitializeCore`
	on [shutdown] is native `LLVMShutdown`
	on [createMessage: (Str)] (Str) is native `LLVMCreateMessage`
	on [disposeMessage: (Str)] is native `LLVMDisposeMessage`
		
	;-- Contexts
	on [contextCreate] (ContextRef) is native `LLVMContextCreate`
	on [getGlobalContext] (ContextRef) is native `LLVMGetGlobalContext`

	on [contextSetDiagnosticHandler: (ContextRef), (DiagnosticHandler), (Ptr[Void])] is native `LLVMContextSetDiagnosticHandler`
	on [contextGetDiagnosticHandler: (ContextRef)] (DiagnosticHandler) is native `LLVMContextGetDiagnosticHandler`
	on [contextGetDiagnosticContext: (ContextRef)] (Ptr[Void]) is native `LLVMContextGetDiagnosticContext`

	on [contextSetYieldCallback: (ContextRef), (YieldCallback), (Ptr[Void])] is native `LLVMContextSetYieldCallback`
	on [contextShouldDiscardValueNames: (ContextRef)] (Bool) is native `LLVMContextShouldDiscardValueNames`
	on [contextSetDiscardValueNames: (ContextRef), (Bool)] is native `LLVMContextSetDiscardValueNames`

	on [contextDispose: (ContextRef)] is native `LLVMContextDispose`

	on [getDiagInfoDescription: (DiagnosticInfoRef)] (Str) is native `LLVMGetDiagInfoDescription`
	on [getDiagInfoSeverity: (DiagnosticInfoRef)] (DiagnosticSeverity) is native `LLVMGetDiagInfoSeverity`

	on [getMDKindIDInContext: (ContextRef), (Str), (Size_t)] (Size_t) is native `LLVMGetMDKindIDInContext`
	on [getMDKindID: (Str), (Size_t)] (Size_t) is native `LLVMGetMDKindID`
	on [getEnumAttributeKindForName: (Str), (Size_t)] (Size_t) is native `LLVMGetEnumAttributeKindForName`
	on [getLastEnumAttributeKind] (Size_t) is native `LLVMGetLastEnumAttributeKind`
	on [createEnumAttribute: (ContextRef), (Size_t), (UInt64)] (AttributeRef) is native `LLVMCreateEnumAttribute`
	on [getEnumAttributeKind: (AttributeRef)] (Size_t) is native `LLVMGetEnumAttributeKind`
	on [getEnumAttributeValue: (AttributeRef)] (UInt64) is native `LLVMGetEnumAttributeValue`
	on [createStringAttribute: (ContextRef), (Str), (Size_t), (Str), (Size_t)] (AttributeRef) is native `LLVMCreateStringAttribute`
	on [getStringAttributeKind: (AttributeRef), (Ptr[Size_t])] (Str) is native `LLVMGetStringAttributeKind`
	on [getStringAttributeValue: (AttributeRef), (Ptr[Size_t])] (Str) is native `LLVMGetStringAttributeValue`
	on [isEnumAttribute: (AttributeRef)] (Bool) is native `LLVMIsEnumAttribute`
	on [isStringAttribute: (AttributeRef)] (Bool) is native `LLVMIsStringAttribute`
	
	;-- Modules
	on [moduleCreateWithName: (Str)] (ModuleRef) is native `LLVMModuleCreateWithName`
	on [moduleCreateWithNameInContext: (Str), (ContextRef)] (ModuleRef) is native `LLVMModuleCreateWithNameInContext`
	on [cloneModule: (ModuleRef)] (ModuleRef) is native `LLVMCloneModule`
	on [disposeModule: (ModuleRef)] is native `LLVMDisposeModule`
	on [getModuleIdentifier: (ModuleRef), (Ptr[Size_t])] (Str) is native `LLVMGetModuleIdentifier`
	on [setModuleIdentifier: (ModuleRef), (Str), (Size_t)] is native `LLVMSetModuleIdentifier`
	on [getSourceFileName: (ModuleRef), (Ptr[Size_t])] (Str) is native `LLVMGetSourceFileName`
	on [setSourceFileName: (ModuleRef), (Str), (Size_t)] is native `LLVMSetSourceFileName`
	on [getDataLayoutStr: (ModuleRef)] (Str) is native `LLVMGetDataLayoutStr`
	on [getDataLayout: (ModuleRef)] (Str) is native `LLVMGetDataLayout`
	on [setDataLayout: (ModuleRef), (Str)] is native `LLVMSetDataLayout`
	on [getTarget: (ModuleRef)] (Str) is native `LLVMGetTarget`
	on [setTarget: (ModuleRef), (Str)] is native `LLVMSetTarget`

	on [copyModuleFlagsMetadata: (ModuleRef), (Ptr[Size_t])] (Array[Int32]) is native `LLVMCopyModuleFlagsMetadata`
	on [disposeModuleFlagsMetadata: (Array[Int32])] is native `LLVMDisposeModuleFlagsMetadata`
	on [moduleFlagEntriesGetFlagBehavior: (Array[Int32]), (Size_t)] (Int32) is native `LLVMModuleFlagEntriesGetFlagBehavior`
	on [moduleFlagEntriesGetKey: (Array[Int32]), (Size_t), (Ptr[Size_t])] (Str) is native `LLVMModuleFlagEntriesGetKey`

	on [moduleFlagEntriesGetMetadata: (Array[Int32]), (Size_t)] (MetadataRef) is native `LLVMModuleFlagEntriesGetMetadata`
	on [getModuleFlag: (ModuleRef), (Str), (Size_t)] (MetadataRef) is native `LLVMGetModuleFlag`
	on [addModuleFlag: (ModuleRef), (Int32), (Str), (Size_t), (MetadataRef)] is native `LLVMAddModuleFlag`
	on [dumpModule: (ModuleRef)] is native `LLVMDumpModule`
	on [printModuleToFile: (ModuleRef), (Str), (Ptr[Str])] (Int32) is native `LLVMPrintModuleToFile`
	on [printModuleToString: (ModuleRef)] (Str) is native `LLVMPrintModuleToString`

	on [getModuleInlineAsm: (ModuleRef), (Ptr[Size_t])] (Str) is native `LLVMGetModuleInlineAsm`
	on [setModuleInlineAsm2: (ModuleRef), (Str), (Size_t)] is native `LLVMSetModuleInlineAsm2`
	on [appendModuleInlineAsm: (ModuleRef), (Str), (Size_t)] is native `LLVMAppendModuleInlineAsm`
	on [getInlineAsm: (TypeRef), (Str), (Size_t), (Str), (Size_t), (Bool), (Bool), (InlineAsmDialect)] (ValueRef) is native `LLVMGetInlineAsm`
	on [getModuleContext: (ModuleRef)] (ContextRef) is native `LLVMGetModuleContext`
	on [getTypeByName: (ModuleRef), (Str)] (TypeRef) is native `LLVMGetTypeByName`

	on [getFirstNamedMetadata: (ModuleRef)] (NamedMDNodeRef) is native `LLVMGetFirstNamedMetadata`
	on [getLastNamedMetadata: (ModuleRef)] (NamedMDNodeRef) is native `LLVMGetLastNamedMetadata`
	on [getNextNamedMetadata: (NamedMDNodeRef)] (NamedMDNodeRef) is native `LLVMGetNextNamedMetadata`
	on [getPreviousNamedMetadata: (NamedMDNodeRef)] (NamedMDNodeRef) is native `LLVMGetPreviousNamedMetadata`
	on [getNamedMetadata: (ModuleRef), (Str), (Size_t)] (NamedMDNodeRef) is native `LLVMGetNamedMetadata`
	on [getOrInsertNamedMetadata: (ModuleRef), (Str), (Size_t)] (NamedMDNodeRef) is native `LLVMGetOrInsertNamedMetadata`
	on [getNamedMetadataName: (NamedMDNodeRef), (Ptr[Size_t])] (Str) is native `LLVMGetNamedMetadataName`
	on [getNamedMetadataNumOperands: (ModuleRef), (Str)] (Size_t) is native `LLVMGetNamedMetadataNumOperands`
	on [getNamedMetadataOperands: (ModuleRef), (Str), (Array[ValueRef])] is native `LLVMGetNamedMetadataOperands`
	on [addNamedMetadataOperand: (ModuleRef), (Str), (ValueRef)] is native `LLVMAddNamedMetadataOperand`

	on [getDebugLocDirectory: (ValueRef), (Ptr[Size_t])] (Str) is native `LLVMGetDebugLocDirectory`
	on [getDebugLocFilename: (ValueRef), (Ptr[Size_t])] (Str) is native `LLVMGetDebugLocFilename`
	on [getDebugLocLine: (ValueRef)] (Size_t) is native `LLVMGetDebugLocLine`
	on [getDebugLocColumn: (ValueRef)] (Size_t) is native `LLVMGetDebugLocColumn`
	on [addFunction: (ModuleRef), (Str), (TypeRef)] (ValueRef) is native `LLVMAddFunction`
	on [getNamedFunction: (ModuleRef), (Str)] (ValueRef) is native `LLVMGetNamedFunction`
	on [getFirstFunction: (ModuleRef)] (ValueRef) is native `LLVMGetFirstFunction`
	on [getLastFunction: (ModuleRef)] (ValueRef) is native `LLVMGetLastFunction`
	on [getNextFunction: (ValueRef)] (ValueRef) is native `LLVMGetNextFunction`
	on [getPreviousFunction: (ValueRef)] (ValueRef) is native `LLVMGetPreviousFunction`
	on [setModuleInlineAsm: (ModuleRef), (Str)] is native `LLVMSetModuleInlineAsm`
	
	
	;-- Types
	on [getTypeKind: (TypeRef)] (TypeKind) is native `LLVMGetTypeKind`
	on [typeIsSized: (TypeRef)] (Bool) is native `LLVMTypeIsSized`
	on [getTypeContext: (TypeRef)] (ContextRef) is native `LLVMGetTypeContext`
	on [dumpType: (TypeRef)] is native `LLVMDumpType`
	on [printTypeToString: (TypeRef)] (Str) is native `LLVMPrintTypeToString`

	on [int1TypeInContext: (ContextRef)] (TypeRef) is native `LLVMInt1TypeInContext`
	on [int8TypeInContext: (ContextRef)] (TypeRef) is native `LLVMInt8TypeInContext`
	on [int16TypeInContext: (ContextRef)] (TypeRef) is native `LLVMInt16TypeInContext`
	on [int32TypeInContext: (ContextRef)] (TypeRef) is native `LLVMInt32TypeInContext`
	on [int64TypeInContext: (ContextRef)] (TypeRef) is native `LLVMInt64TypeInContext`
	on [int128TypeInContext: (ContextRef)] (TypeRef) is native `LLVMInt128TypeInContext`
	on [intTypeInContext: (ContextRef), (Size_t)] (TypeRef) is native `LLVMIntTypeInContext`
	on [int1Type] (TypeRef) is native `LLVMInt1Type`
	on [int8Type] (TypeRef) is native `LLVMInt8Type`
	on [int16Type] (TypeRef) is native `LLVMInt16Type`
	on [int32Type] (TypeRef) is native `LLVMInt32Type`
	on [int64Type] (TypeRef) is native `LLVMInt64Type`
	on [int128Type] (TypeRef) is native `LLVMInt128Type`
	on [intType: (Size_t)] (TypeRef) is native `LLVMIntType`
	on [getIntTypeWidth: (TypeRef)] (Size_t) is native `LLVMGetIntTypeWidth`

	on [halfTypeInContext: (ContextRef)] (TypeRef) is native `LLVMHalfTypeInContext`
	on [floatTypeInContext: (ContextRef)] (TypeRef) is native `LLVMFloatTypeInContext`
	on [doubleTypeInContext: (ContextRef)] (TypeRef) is native `LLVMDoubleTypeInContext`
	on [x86FP80TypeInContext: (ContextRef)] (TypeRef) is native `LLVMX86FP80TypeInContext`
	on [fp128TypeInContext: (ContextRef)] (TypeRef) is native `LLVMFP128TypeInContext`
	on [ppcFP128TypeInContext: (ContextRef)] (TypeRef) is native `LLVMPPCFP128TypeInContext`
	on [halfType] (TypeRef) is native `LLVMHalfType`
	on [floatType] (TypeRef) is native `LLVMFloatType`
	on [doubleType] (TypeRef) is native `LLVMDoubleType`
	on [x86FP80Type] (TypeRef) is native `LLVMX86FP80Type`
	on [fp128Type] (TypeRef) is native `LLVMFP128Type`
	on [ppcFP128Type] (TypeRef) is native `LLVMPPCFP128Type`

	on [functionType: (TypeRef), (Array[TypeRef]), (Size_t), (Bool)] (TypeRef) is native `LLVMFunctionType`
	on [isFunctionVarArg: (TypeRef)] (Bool) is native `LLVMIsFunctionVarArg`
	on [getReturnType: (TypeRef)] (TypeRef) is native `LLVMGetReturnType`
	on [countParamTypes: (TypeRef)] (Size_t) is native `LLVMCountParamTypes`
	on [getParamTypes: (TypeRef), (Array[TypeRef])] is native `LLVMGetParamTypes`
	
	on [structTypeInContext: (ContextRef), (Array[TypeRef]), (Size_t), (Bool)] (TypeRef) is native `LLVMStructTypeInContext`
	on [structType: (Array[TypeRef]), (Size_t), (Bool)] (TypeRef) is native `LLVMStructType`
	on [structCreateNamed: (ContextRef), (Str)] (TypeRef) is native `LLVMStructCreateNamed`
	on [getStructName: (TypeRef)] (Str) is native `LLVMGetStructName`
	on [structSetBody: (TypeRef), (Array[TypeRef]), (Size_t), (Bool)] is native `LLVMStructSetBody`
	on [countStructElementTypes: (TypeRef)] (Size_t) is native `LLVMCountStructElementTypes`
	on [getStructElementTypes: (TypeRef), (Array[TypeRef])] is native `LLVMGetStructElementTypes`
	on [structGetTypeAtIndex: (TypeRef), (Size_t)] (TypeRef) is native `LLVMStructGetTypeAtIndex`
	on [isPackedStruct: (TypeRef)] (Bool) is native `LLVMIsPackedStruct`
	on [isOpaqueStruct: (TypeRef)] (Bool) is native `LLVMIsOpaqueStruct`
	on [isLiteralStruct: (TypeRef)] (Bool) is native `LLVMIsLiteralStruct`

	on [getElementType: (TypeRef)] (TypeRef) is native `LLVMGetElementType`
	on [getSubtypes: (TypeRef), (Array[TypeRef])] is native `LLVMGetSubtypes`
	on [getNumContainedTypes: (TypeRef)] (Size_t) is native `LLVMGetNumContainedTypes`
	on [arrayType: (TypeRef), (Size_t)] (TypeRef) is native `LLVMArrayType`
	on [getArrayLength: (TypeRef)] (Size_t) is native `LLVMGetArrayLength`
	on [pointerType: (TypeRef), (Size_t)] (TypeRef) is native `LLVMPointerType`
	on [getPointerAddressSpace: (TypeRef)] (Size_t) is native `LLVMGetPointerAddressSpace`
	on [vectorType: (TypeRef), (Size_t)] (TypeRef) is native `LLVMVectorType`
	on [getVectorSize: (TypeRef)] (Size_t) is native `LLVMGetVectorSize`

	on [voidTypeInContext: (ContextRef)] (TypeRef) is native `LLVMVoidTypeInContext`
	on [labelTypeInContext: (ContextRef)] (TypeRef) is native `LLVMLabelTypeInContext`
	on [x86MMXTypeInContext: (ContextRef)] (TypeRef) is native `LLVMX86MMXTypeInContext`
	on [tokenTypeInContext: (ContextRef)] (TypeRef) is native `LLVMTokenTypeInContext`
	on [metadataTypeInContext: (ContextRef)] (TypeRef) is native `LLVMMetadataTypeInContext`
	on [voidType] (TypeRef) is native `LLVMVoidType`
	on [labelType] (TypeRef) is native `LLVMLabelType`
	on [x86MMXType] (TypeRef) is native `LLVMX86MMXType`
	
	;-- Values
	on [typeOf: (ValueRef)] (TypeRef) is native `LLVMTypeOf`
	on [getValueKind: (ValueRef)] (ValueKind) is native `LLVMGetValueKind`
	on [getValueName2: (ValueRef), (Ptr[Size_t])] (Str) is native `LLVMGetValueName2`
	on [setValueName2: (ValueRef), (Str), (Size_t)] is native `LLVMSetValueName2`
	on [dumpValue: (ValueRef)] is native `LLVMDumpValue`
	on [printValueToString: (ValueRef)] (Str) is native `LLVMPrintValueToString`
	on [replaceAllUsesWith: (ValueRef), (ValueRef)] is native `LLVMReplaceAllUsesWith`
	on [isConstant: (ValueRef)] (Bool) is native `LLVMIsConstant`
	on [isUndef: (ValueRef)] (Bool) is native `LLVMIsUndef`
	on [isAMDNode: (ValueRef)] (ValueRef) is native `LLVMIsAMDNode`
	on [isAMDString: (ValueRef)] (ValueRef) is native `LLVMIsAMDString`
	on [getValueName: (ValueRef)] (Str) is native `LLVMGetValueName`
	on [setValueName: (ValueRef), (Str)] is native `LLVMSetValueName`

	on [getFirstUse: (ValueRef)] (UseRef) is native `LLVMGetFirstUse`
	on [getNextUse: (UseRef)] (UseRef) is native `LLVMGetNextUse`
	on [getUser: (UseRef)] (ValueRef) is native `LLVMGetUser`
	on [getUsedValue: (UseRef)] (ValueRef) is native `LLVMGetUsedValue`

	on [getOperand: (ValueRef), (Size_t)] (ValueRef) is native `LLVMGetOperand`
	on [getOperandUse: (ValueRef), (Size_t)] (UseRef) is native `LLVMGetOperandUse`
	on [setOperand: (ValueRef), (Size_t), (ValueRef)] is native `LLVMSetOperand`
	on [getNumOperands: (ValueRef)] (Int32) is native `LLVMGetNumOperands`

	on [constNull: (TypeRef)] (ValueRef) is native `LLVMConstNull`
	on [constAllOnes: (TypeRef)] (ValueRef) is native `LLVMConstAllOnes`
	on [getUndef: (TypeRef)] (ValueRef) is native `LLVMGetUndef`
	on [isNull: (ValueRef)] (Bool) is native `LLVMIsNull`
	on [constPointerNull: (TypeRef)] (ValueRef) is native `LLVMConstPointerNull`
	
	on [constInt: (TypeRef), (UInt64), (Bool)] (ValueRef) is native `LLVMConstInt`
	on [constIntOfArbitraryPrecision: (TypeRef), (Size_t), (Array[UInt64])] (ValueRef) is native `LLVMConstIntOfArbitraryPrecision`
	on [constIntOfString: (TypeRef), (Str), (UInt8)] (ValueRef) is native `LLVMConstIntOfString`
	on [constIntOfStringAndSize: (TypeRef), (Str), (Size_t), (UInt8)] (ValueRef) is native `LLVMConstIntOfStringAndSize`
	on [constReal: (TypeRef), (Dec64)] (ValueRef) is native `LLVMConstReal`
	on [constRealOfString: (TypeRef), (Str)] (ValueRef) is native `LLVMConstRealOfString`
	on [constRealOfStringAndSize: (TypeRef), (Str), (Size_t)] (ValueRef) is native `LLVMConstRealOfStringAndSize`
	on [constIntGetZExtValue: (ValueRef)] (UInt64) is native `LLVMConstIntGetZExtValue`
	on [constIntGetSExtValue: (ValueRef)] (Int64) is native `LLVMConstIntGetSExtValue`
	on [constRealGetDouble: (ValueRef), (Ptr[Int32])] (Dec64) is native `LLVMConstRealGetDouble`

	on [constStringInContext: (ContextRef), (Str), (Size_t), (Bool)] (ValueRef) is native `LLVMConstStringInContext`
	on [constString: (Str), (Size_t), (Bool)] (ValueRef) is native `LLVMConstString`
	on [isConstantString: (ValueRef)] (Bool) is native `LLVMIsConstantString`
	on [getAsString: (ValueRef), (Ptr[Size_t])] (Str) is native `LLVMGetAsString`
	on [constStructInContext: (ContextRef), (Array[ValueRef]), (Size_t), (Bool)] (ValueRef) is native `LLVMConstStructInContext`
	on [constStruct: (Array[ValueRef]), (Size_t), (Bool)] (ValueRef) is native `LLVMConstStruct`
	on [constArray: (TypeRef), (Array[ValueRef]), (Size_t)] (ValueRef) is native `LLVMConstArray`
	on [constNamedStruct: (TypeRef), (Array[ValueRef]), (Size_t)] (ValueRef) is native `LLVMConstNamedStruct`
	on [getElementAsConstant: (ValueRef), (Size_t)] (ValueRef) is native `LLVMGetElementAsConstant`
	on [constVector: (Array[ValueRef]), (Size_t)] (ValueRef) is native `LLVMConstVector`

	on [getConstOpcode: (ValueRef)] (Opcode) is native `LLVMGetConstOpcode`
	on [alignOf: (TypeRef)] (ValueRef) is native `LLVMAlignOf`
	on [sizeOf: (TypeRef)] (ValueRef) is native `LLVMSizeOf`
	on [constNeg: (ValueRef)] (ValueRef) is native `LLVMConstNeg`
	on [constNSWNeg: (ValueRef)] (ValueRef) is native `LLVMConstNSWNeg`
	on [constNUWNeg: (ValueRef)] (ValueRef) is native `LLVMConstNUWNeg`
	on [constFNeg: (ValueRef)] (ValueRef) is native `LLVMConstFNeg`
	on [constNot: (ValueRef)] (ValueRef) is native `LLVMConstNot`
	on [constAdd: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstAdd`
	on [constNSWAdd: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstNSWAdd`
	on [constNUWAdd: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstNUWAdd`
	on [constFAdd: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstFAdd`
	on [constSub: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstSub`
	on [constNSWSub: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstNSWSub`
	on [constNUWSub: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstNUWSub`
	on [constFSub: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstFSub`
	on [constMul: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstMul`
	on [constNSWMul: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstNSWMul`
	on [constNUWMul: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstNUWMul`
	on [constFMul: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstFMul`
	on [constUDiv: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstUDiv`
	on [constExactUDiv: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstExactUDiv`
	on [constSDiv: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstSDiv`
	on [constExactSDiv: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstExactSDiv`
	on [constFDiv: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstFDiv`
	on [constURem: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstURem`
	on [constSRem: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstSRem`
	on [constFRem: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstFRem`
	on [constAnd: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstAnd`
	on [constOr: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstOr`
	on [constXor: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstXor`
	on [constICmp: (Int32), (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstICmp`
	on [constFCmp: (Int32), (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstFCmp`
	on [constShl: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstShl`
	on [constLShr: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstLShr`
	on [constAShr: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstAShr`
	on [constGEP: (ValueRef), (Array[ValueRef]), (Size_t)] (ValueRef) is native `LLVMConstGEP`
	on [constGEP2: (TypeRef), (ValueRef), (Array[ValueRef]), (Size_t)] (ValueRef) is native `LLVMConstGEP2`
	on [constInBoundsGEP: (ValueRef), (Array[ValueRef]), (Size_t)] (ValueRef) is native `LLVMConstInBoundsGEP`
	on [constInBoundsGEP2: (TypeRef), (ValueRef), (Array[ValueRef]), (Size_t)] (ValueRef) is native `LLVMConstInBoundsGEP2`
	on [constTrunc: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstTrunc`
	on [constSExt: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstSExt`
	on [constZExt: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstZExt`
	on [constFPTrunc: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstFPTrunc`
	on [constFPExt: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstFPExt`
	on [constUIToFP: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstUIToFP`
	on [constSIToFP: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstSIToFP`
	on [constFPToUI: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstFPToUI`
	on [constFPToSI: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstFPToSI`
	on [constPtrToInt: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstPtrToInt`
	on [constIntToPtr: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstIntToPtr`
	on [constBitCast: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstBitCast`
	on [constAddrSpaceCast: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstAddrSpaceCast`
	on [constZExtOrBitCast: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstZExtOrBitCast`
	on [constSExtOrBitCast: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstSExtOrBitCast`
	on [constTruncOrBitCast: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstTruncOrBitCast`
	on [constPointerCast: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstPointerCast`
	on [constIntCast: (ValueRef), (TypeRef), (Bool)] (ValueRef) is native `LLVMConstIntCast`
	on [constFPCast: (ValueRef), (TypeRef)] (ValueRef) is native `LLVMConstFPCast`
	on [constSelect: (ValueRef), (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstSelect`
	on [constExtractElement: (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstExtractElement`
	on [constInsertElement: (ValueRef), (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstInsertElement`
	on [constShuffleVector: (ValueRef), (ValueRef), (ValueRef)] (ValueRef) is native `LLVMConstShuffleVector`
	on [constExtractValue: (ValueRef), (Array[Size_t]), (Size_t)] (ValueRef) is native `LLVMConstExtractValue`
	on [constInsertValue: (ValueRef), (ValueRef), (Array[Size_t]), (Size_t)] (ValueRef) is native `LLVMConstInsertValue`
	on [blockAddress: (ValueRef), (BasicBlockRef)] (ValueRef) is native `LLVMBlockAddress`
	on [constInlineAsm: (TypeRef), (Str), (Str), (Bool), (Bool)] (ValueRef) is native `LLVMConstInlineAsm`

	on [getGlobalParent: (ValueRef)] (ModuleRef) is native `LLVMGetGlobalParent`
	on [isDeclaration: (ValueRef)] (Bool) is native `LLVMIsDeclaration`
	on [getLinkage: (ValueRef)] (Linkage) is native `LLVMGetLinkage`
	on [setLinkage: (ValueRef), (Linkage)] is native `LLVMSetLinkage`
	on [getSection: (ValueRef)] (Str) is native `LLVMGetSection`
	on [setSection: (ValueRef), (Str)] is native `LLVMSetSection`
	on [getVisibility: (ValueRef)] (Visibility) is native `LLVMGetVisibility`
	on [setVisibility: (ValueRef), (Visibility)] is native `LLVMSetVisibility`
	on [getDLLStorageClass: (ValueRef)] (DLLStorageClass) is native `LLVMGetDLLStorageClass`
	on [setDLLStorageClass: (ValueRef), (DLLStorageClass)] is native `LLVMSetDLLStorageClass`
	on [getUnnamedAddress: (ValueRef)] (UnnamedAddr) is native `LLVMGetUnnamedAddress`
	on [setUnnamedAddress: (ValueRef), (UnnamedAddr)] is native `LLVMSetUnnamedAddress`
	on [globalGetValueType: (ValueRef)] (TypeRef) is native `LLVMGlobalGetValueType`
	on [hasUnnamedAddr: (ValueRef)] (Bool) is native `LLVMHasUnnamedAddr`
	on [setUnnamedAddr: (ValueRef), (Bool)] is native `LLVMSetUnnamedAddr`
	on [getAlignment: (ValueRef)] (Size_t) is native `LLVMGetAlignment`
	on [setAlignment: (ValueRef), (Size_t)] is native `LLVMSetAlignment`
	on [globalSetMetadata: (ValueRef), (Size_t), (MetadataRef)] is native `LLVMGlobalSetMetadata`
	on [globalEraseMetadata: (ValueRef), (Size_t)] is native `LLVMGlobalEraseMetadata`
	on [globalClearMetadata: (ValueRef)] is native `LLVMGlobalClearMetadata`
	on [globalCopyAllMetadata: (ValueRef), (Ptr[Size_t])] (Array[ValueMetadataEntry]) is native `LLVMGlobalCopyAllMetadata`
	on [disposeValueMetadataEntries: (Array[ValueMetadataEntry])] is native `LLVMDisposeValueMetadataEntries`
	on [valueMetadataEntriesGetKind: (Array[ValueMetadataEntry]), (Size_t)] (Size_t) is native `LLVMValueMetadataEntriesGetKind`
	on [valueMetadataEntriesGetMetadata: (Array[ValueMetadataEntry]), (Size_t)] (MetadataRef) is native `LLVMValueMetadataEntriesGetMetadata`

	on [addGlobal: (ModuleRef), (TypeRef), (Str)] (ValueRef) is native `LLVMAddGlobal`
	on [addGlobalInAddressSpace: (ModuleRef), (TypeRef), (Str), (Size_t)] (ValueRef) is native `LLVMAddGlobalInAddressSpace`
	on [getNamedGlobal: (ModuleRef), (Str)] (ValueRef) is native `LLVMGetNamedGlobal`
	on [getFirstGlobal: (ModuleRef)] (ValueRef) is native `LLVMGetFirstGlobal`
	on [getLastGlobal: (ModuleRef)] (ValueRef) is native `LLVMGetLastGlobal`
	on [getNextGlobal: (ValueRef)] (ValueRef) is native `LLVMGetNextGlobal`
	on [getPreviousGlobal: (ValueRef)] (ValueRef) is native `LLVMGetPreviousGlobal`
	on [deleteGlobal: (ValueRef)] is native `LLVMDeleteGlobal`
	on [getInitializer: (ValueRef)] (ValueRef) is native `LLVMGetInitializer`
	on [setInitializer: (ValueRef), (ValueRef)] is native `LLVMSetInitializer`
	on [isThreadLocal: (ValueRef)] (Bool) is native `LLVMIsThreadLocal`
	on [setThreadLocal: (ValueRef), (Bool)] is native `LLVMSetThreadLocal`
	on [isGlobalConstant: (ValueRef)] (Bool) is native `LLVMIsGlobalConstant`
	on [setGlobalConstant: (ValueRef), (Bool)] is native `LLVMSetGlobalConstant`
	on [getThreadLocalMode: (ValueRef)] (ThreadLocalMode) is native `LLVMGetThreadLocalMode`
	on [setThreadLocalMode: (ValueRef), (ThreadLocalMode)] is native `LLVMSetThreadLocalMode`
	on [isExternallyInitialized: (ValueRef)] (Bool) is native `LLVMIsExternallyInitialized`
	on [setExternallyInitialized: (ValueRef), (Bool)] is native `LLVMSetExternallyInitialized`

	on [addAlias: (ModuleRef), (TypeRef), (ValueRef), (Str)] (ValueRef) is native `LLVMAddAlias`
	on [getNamedGlobalAlias: (ModuleRef), (Str), (Size_t)] (ValueRef) is native `LLVMGetNamedGlobalAlias`
	on [getFirstGlobalAlias: (ModuleRef)] (ValueRef) is native `LLVMGetFirstGlobalAlias`
	on [getLastGlobalAlias: (ModuleRef)] (ValueRef) is native `LLVMGetLastGlobalAlias`
	on [getNextGlobalAlias: (ValueRef)] (ValueRef) is native `LLVMGetNextGlobalAlias`
	on [getPreviousGlobalAlias: (ValueRef)] (ValueRef) is native `LLVMGetPreviousGlobalAlias`
	on [aliasGetAliasee: (ValueRef)] (ValueRef) is native `LLVMAliasGetAliasee`
	on [aliasSetAliasee: (ValueRef), (ValueRef)] is native `LLVMAliasSetAliasee`
	
	on [countParams: (ValueRef)] (Size_t) is native `LLVMCountParams`
	on [getParams: (ValueRef), (Array[ValueRef])] is native `LLVMGetParams`
	on [getParam: (ValueRef), (Size_t)] (ValueRef) is native `LLVMGetParam`
	on [getParamParent: (ValueRef)] (ValueRef) is native `LLVMGetParamParent`
	on [getFirstParam: (ValueRef)] (ValueRef) is native `LLVMGetFirstParam`
	on [getLastParam: (ValueRef)] (ValueRef) is native `LLVMGetLastParam`
	on [getNextParam: (ValueRef)] (ValueRef) is native `LLVMGetNextParam`
	on [getPreviousParam: (ValueRef)] (ValueRef) is native `LLVMGetPreviousParam`
	on [setParamAlignment: (ValueRef), (Size_t)] is native `LLVMSetParamAlignment`

	on [addGlobalIFunc: (ModuleRef), (Str), (Size_t), (TypeRef), (Size_t), (ValueRef)] (ValueRef) is native `LLVMAddGlobalIFunc`
	on [getNamedGlobalIFunc: (ModuleRef), (Str), (Size_t)] (ValueRef) is native `LLVMGetNamedGlobalIFunc`
	on [getFirstGlobalIFunc: (ModuleRef)] (ValueRef) is native `LLVMGetFirstGlobalIFunc`
	on [getLastGlobalIFunc: (ModuleRef)] (ValueRef) is native `LLVMGetLastGlobalIFunc`
	on [getNextGlobalIFunc: (ValueRef)] (ValueRef) is native `LLVMGetNextGlobalIFunc`
	on [getPreviousGlobalIFunc: (ValueRef)] (ValueRef) is native `LLVMGetPreviousGlobalIFunc`
	on [getGlobalIFuncResolver: (ValueRef)] (ValueRef) is native `LLVMGetGlobalIFuncResolver`
	on [setGlobalIFuncResolver: (ValueRef), (ValueRef)] is native `LLVMSetGlobalIFuncResolver`
	on [eraseGlobalIFunc: (ValueRef)] is native `LLVMEraseGlobalIFunc`
	on [removeGlobalIFunc: (ValueRef)] is native `LLVMRemoveGlobalIFunc`

	on [deleteFunction: (ValueRef)] is native `LLVMDeleteFunction`
	on [hasPersonalityFn: (ValueRef)] (Bool) is native `LLVMHasPersonalityFn`
	on [getPersonalityFn: (ValueRef)] (ValueRef) is native `LLVMGetPersonalityFn`
	on [setPersonalityFn: (ValueRef), (ValueRef)] is native `LLVMSetPersonalityFn`
	on [lookupIntrinsicID: (Str), (Size_t)] (Size_t) is native `LLVMLookupIntrinsicID`
	on [getIntrinsicID: (ValueRef)] (Size_t) is native `LLVMGetIntrinsicID`
	on [getIntrinsicDeclaration: (ModuleRef), (Size_t), (Array[TypeRef]), (Size_t)] (ValueRef) is native `LLVMGetIntrinsicDeclaration`
	on [intrinsicGetType: (ContextRef), (Size_t), (Array[TypeRef]), (Size_t)] (TypeRef) is native `LLVMIntrinsicGetType`
	on [intrinsicGetName: (Size_t), (Ptr[Size_t])] (Str) is native `LLVMIntrinsicGetName`
	on [intrinsicCopyOverloadedName: (Size_t), (Array[TypeRef]), (Size_t), (Ptr[Size_t])] (Str) is native `LLVMIntrinsicCopyOverloadedName`
	on [intrinsicIsOverloaded: (Size_t)] (Bool) is native `LLVMIntrinsicIsOverloaded`
	on [getFunctionCallConv: (ValueRef)] (Size_t) is native `LLVMGetFunctionCallConv`
	on [setFunctionCallConv: (ValueRef), (Size_t)] is native `LLVMSetFunctionCallConv`
	on [getGC: (ValueRef)] (Str) is native `LLVMGetGC`
	on [setGC: (ValueRef), (Str)] is native `LLVMSetGC`
	on [addAttributeAtIndex: (ValueRef), (Size_t), (AttributeRef)] is native `LLVMAddAttributeAtIndex`
	on [getAttributeCountAtIndex: (ValueRef), (Size_t)] (Size_t) is native `LLVMGetAttributeCountAtIndex`
	on [getAttributesAtIndex: (ValueRef), (Size_t), (Array[AttributeRef])] is native `LLVMGetAttributesAtIndex`
	on [getEnumAttributeAtIndex: (ValueRef), (Size_t), (Size_t)] (AttributeRef) is native `LLVMGetEnumAttributeAtIndex`
	on [getStringAttributeAtIndex: (ValueRef), (Size_t), (Str), (Size_t)] (AttributeRef) is native `LLVMGetStringAttributeAtIndex`
	on [removeEnumAttributeAtIndex: (ValueRef), (Size_t), (Size_t)] is native `LLVMRemoveEnumAttributeAtIndex`
	on [removeStringAttributeAtIndex: (ValueRef), (Size_t), (Str), (Size_t)] is native `LLVMRemoveStringAttributeAtIndex`
	on [addTargetDependentFunctionAttr: (ValueRef), (Str), (Str)] is native `LLVMAddTargetDependentFunctionAttr`
	
	;-- Basic Blocks
	on [basicBlockAsValue: (BasicBlockRef)] (ValueRef) is native `LLVMBasicBlockAsValue`
	on [valueIsBasicBlock: (ValueRef)] (Bool) is native `LLVMValueIsBasicBlock`
	on [valueAsBasicBlock: (ValueRef)] (BasicBlockRef) is native `LLVMValueAsBasicBlock`
	on [getBasicBlockName: (BasicBlockRef)] (Str) is native `LLVMGetBasicBlockName`
	on [getBasicBlockParent: (BasicBlockRef)] (ValueRef) is native `LLVMGetBasicBlockParent`
	on [getBasicBlockTerminator: (BasicBlockRef)] (ValueRef) is native `LLVMGetBasicBlockTerminator`
	on [countBasicBlocks: (ValueRef)] (Size_t) is native `LLVMCountBasicBlocks`
	on [getBasicBlocks: (ValueRef), (Array[BasicBlockRef])] is native `LLVMGetBasicBlocks`
	on [getFirstBasicBlock: (ValueRef)] (BasicBlockRef) is native `LLVMGetFirstBasicBlock`
	on [getLastBasicBlock: (ValueRef)] (BasicBlockRef) is native `LLVMGetLastBasicBlock`
	on [getNextBasicBlock: (BasicBlockRef)] (BasicBlockRef) is native `LLVMGetNextBasicBlock`
	on [getPreviousBasicBlock: (BasicBlockRef)] (BasicBlockRef) is native `LLVMGetPreviousBasicBlock`
	on [getEntryBasicBlock: (ValueRef)] (BasicBlockRef) is native `LLVMGetEntryBasicBlock`
	on [insertExistingBasicBlockAfterInsertBlock: (BuilderRef), (BasicBlockRef)] is native `LLVMInsertExistingBasicBlockAfterInsertBlock`
	on [appendExistingBasicBlock: (ValueRef), (BasicBlockRef)] is native `LLVMAppendExistingBasicBlock`
	on [createBasicBlockInContext: (ContextRef), (Str)] (BasicBlockRef) is native `LLVMCreateBasicBlockInContext`
	on [appendBasicBlockInContext: (ContextRef), (ValueRef), (Str)] (BasicBlockRef) is native `LLVMAppendBasicBlockInContext`
	on [appendBasicBlock: (ValueRef), (Str)] (BasicBlockRef) is native `LLVMAppendBasicBlock`
	on [insertBasicBlockInContext: (ContextRef), (BasicBlockRef), (Str)] (BasicBlockRef) is native `LLVMInsertBasicBlockInContext`
	on [insertBasicBlock: (BasicBlockRef), (Str)] (BasicBlockRef) is native `LLVMInsertBasicBlock`
	on [deleteBasicBlock: (BasicBlockRef)] is native `LLVMDeleteBasicBlock`
	on [removeBasicBlockFromParent: (BasicBlockRef)] is native `LLVMRemoveBasicBlockFromParent`
	on [moveBasicBlockBefore: (BasicBlockRef), (BasicBlockRef)] is native `LLVMMoveBasicBlockBefore`
	on [moveBasicBlockAfter: (BasicBlockRef), (BasicBlockRef)] is native `LLVMMoveBasicBlockAfter`
	on [getFirstInstruction: (BasicBlockRef)] (ValueRef) is native `LLVMGetFirstInstruction`
	on [getLastInstruction: (BasicBlockRef)] (ValueRef) is native `LLVMGetLastInstruction`
	
	;-- Instructions
	on [hasMetadata: (ValueRef)] (Bool) is native `LLVMHasMetadata`
	on [getMetadata: (ValueRef), (Size_t)] (ValueRef) is native `LLVMGetMetadata`
	on [setMetadata: (ValueRef), (Size_t), (ValueRef)] is native `LLVMSetMetadata`
	on [instructionGetAllMetadataOtherThanDebugLoc: (ValueRef), (Ptr[Size_t])] (Array[ValueMetadataEntry]) is native `LLVMInstructionGetAllMetadataOtherThanDebugLoc`
	on [getInstructionParent: (ValueRef)] (BasicBlockRef) is native `LLVMGetInstructionParent`
	on [getNextInstruction: (ValueRef)] (ValueRef) is native `LLVMGetNextInstruction`
	on [getPreviousInstruction: (ValueRef)] (ValueRef) is native `LLVMGetPreviousInstruction`
	on [instructionRemoveFromParent: (ValueRef)] is native `LLVMInstructionRemoveFromParent`
	on [instructionEraseFromParent: (ValueRef)] is native `LLVMInstructionEraseFromParent`
	on [getInstructionOpcode: (ValueRef)] (Opcode) is native `LLVMGetInstructionOpcode`
	on [getICmpPredicate: (ValueRef)] (IntPredicate) is native `LLVMGetICmpPredicate`
	on [getFCmpPredicate: (ValueRef)] (IntPredicate) is native `LLVMGetFCmpPredicate`
	on [instructionClone: (ValueRef)] (ValueRef) is native `LLVMInstructionClone`
	on [isATerminatorInst: (ValueRef)] (ValueRef) is native `LLVMIsATerminatorInst`

	on [getNumArgOperands: (ValueRef)] (Size_t) is native `LLVMGetNumArgOperands`
	on [setInstructionCallConv: (ValueRef), (CallConv)] is native `LLVMSetInstructionCallConv`
	on [getInstructionCallConv: (ValueRef)] (CallConv) is native `LLVMGetInstructionCallConv`
	on [setInstrParamAlignment: (ValueRef), (Size_t), (Size_t)] is native `LLVMSetInstrParamAlignment`
	on [addCallSiteAttribute: (ValueRef), (Size_t), (AttributeRef)] is native `LLVMAddCallSiteAttribute`
	on [getCallSiteAttributeCount: (ValueRef), (Size_t)] (Size_t) is native `LLVMGetCallSiteAttributeCount`
	on [getCallSiteAttributes: (ValueRef), (Size_t), (Array[AttributeRef])] is native `LLVMGetCallSiteAttributes`
	on [getCallSiteEnumAttribute: (ValueRef), (Size_t), (Size_t)] (AttributeRef) is native `LLVMGetCallSiteEnumAttribute`
	on [getCallSiteStringAttribute: (ValueRef), (Size_t), (Str), (Size_t)] (AttributeRef) is native `LLVMGetCallSiteStringAttribute`
	on [removeCallSiteEnumAttribute: (ValueRef), (Size_t), (Size_t)] is native `LLVMRemoveCallSiteEnumAttribute`
	on [removeCallSiteStringAttribute: (ValueRef), (Size_t), (Str), (Size_t)] is native `LLVMRemoveCallSiteStringAttribute`
	on [getCalledFunctionType: (ValueRef)] (TypeRef) is native `LLVMGetCalledFunctionType`
	on [getCalledValue: (ValueRef)] (ValueRef) is native `LLVMGetCalledValue`
	on [isTailCall: (ValueRef)] (Bool) is native `LLVMIsTailCall`
	on [setTailCall: (ValueRef), (Bool)] is native `LLVMSetTailCall`
	on [getNormalDest: (ValueRef)] (BasicBlockRef) is native `LLVMGetNormalDest`
	on [getUnwindDest: (ValueRef)] (BasicBlockRef) is native `LLVMGetUnwindDest`
	on [setNormalDest: (ValueRef), (BasicBlockRef)] is native `LLVMSetNormalDest`
	on [setUnwindDest: (ValueRef), (BasicBlockRef)] is native `LLVMSetUnwindDest`

	on [getNumSuccessors: (ValueRef)] (Size_t) is native `LLVMGetNumSuccessors`
	on [getSuccessor: (ValueRef), (Size_t)] (BasicBlockRef) is native `LLVMGetSuccessor`
	on [setSuccessor: (ValueRef), (Size_t), (BasicBlockRef)] is native `LLVMSetSuccessor`
	on [isConditional: (ValueRef)] (Bool) is native `LLVMIsConditional`
	on [getCondition: (ValueRef)] (ValueRef) is native `LLVMGetCondition`
	on [setCondition: (ValueRef), (ValueRef)] is native `LLVMSetCondition`
	on [getSwitchDefaultDest: (ValueRef)] (BasicBlockRef) is native `LLVMGetSwitchDefaultDest`

	on [getAllocatedType: (ValueRef)] (TypeRef) is native `LLVMGetAllocatedType`

	on [isInBounds: (ValueRef)] (Bool) is native `LLVMIsInBounds`
	on [setIsInBounds: (ValueRef), (Bool)] is native `LLVMSetIsInBounds`

	on [addIncoming: (ValueRef), (Array[ValueRef]), (Array[BasicBlockRef]), (Size_t)] is native `LLVMAddIncoming`
	on [countIncoming: (ValueRef)] (Size_t) is native `LLVMCountIncoming`
	on [getIncomingValue: (ValueRef), (Size_t)] (ValueRef) is native `LLVMGetIncomingValue`
	on [getIncomingBlock: (ValueRef), (Size_t)] (BasicBlockRef) is native `LLVMGetIncomingBlock`

	on [getNumIndices: (ValueRef)] (Size_t) is native `LLVMGetNumIndices`
	on [getIndices: (ValueRef)] (Array[Size_t]) is native `LLVMGetIndices`
	
	;-- Instruction Builder
	on [createBuilderInContext: (ContextRef)] (BuilderRef) is native `LLVMCreateBuilderInContext`
	on [createBuilder] (BuilderRef) is native `LLVMCreateBuilder`
	on [positionBuilder: (BuilderRef), (BasicBlockRef), (ValueRef)] is native `LLVMPositionBuilder`
	on [positionBuilderBefore: (BuilderRef), (ValueRef)] is native `LLVMPositionBuilderBefore`
	on [positionBuilderAtEnd: (BuilderRef), (BasicBlockRef)] is native `LLVMPositionBuilderAtEnd`
	on [getInsertBlock: (BuilderRef)] (BasicBlockRef) is native `LLVMGetInsertBlock`
	on [clearInsertionPosition: (BuilderRef)] is native `LLVMClearInsertionPosition`
	on [insertIntoBuilder: (BuilderRef), (ValueRef)] is native `LLVMInsertIntoBuilder`
	on [insertIntoBuilderWithName: (BuilderRef), (ValueRef), (Str)] is native `LLVMInsertIntoBuilderWithName`
	on [disposeBuilder: (BuilderRef)] is native `LLVMDisposeBuilder`
	on [getCurrentDebugLocation2: (BuilderRef)] (MetadataRef) is native `LLVMGetCurrentDebugLocation2`
	on [setCurrentDebugLocation2: (BuilderRef), (MetadataRef)] is native `LLVMSetCurrentDebugLocation2`
	on [setInstDebugLocation: (BuilderRef), (ValueRef)] is native `LLVMSetInstDebugLocation`
	on [builderGetDefaultFPMathTag: (BuilderRef)] (MetadataRef) is native `LLVMBuilderGetDefaultFPMathTag`
	on [builderSetDefaultFPMathTag: (BuilderRef), (MetadataRef)] is native `LLVMBuilderSetDefaultFPMathTag`
	on [setCurrentDebugLocation: (BuilderRef), (ValueRef)] is native `LLVMSetCurrentDebugLocation`
	on [getCurrentDebugLocation: (BuilderRef)] (ValueRef) is native `LLVMGetCurrentDebugLocation`
	on [buildRetVoid: (BuilderRef)] (ValueRef) is native `LLVMBuildRetVoid`
	on [buildRet: (BuilderRef), (ValueRef)] (ValueRef) is native `LLVMBuildRet`
	on [buildAggregateRet: (BuilderRef), (Array[ValueRef]), (Size_t)] (ValueRef) is native `LLVMBuildAggregateRet`
	on [buildBr: (BuilderRef), (BasicBlockRef)] (ValueRef) is native `LLVMBuildBr`
	on [buildCondBr: (BuilderRef), (ValueRef), (BasicBlockRef), (BasicBlockRef)] (ValueRef) is native `LLVMBuildCondBr`
	on [buildSwitch: (BuilderRef), (ValueRef), (BasicBlockRef), (Size_t)] (ValueRef) is native `LLVMBuildSwitch`
	on [buildIndirectBr: (BuilderRef), (ValueRef), (Size_t)] (ValueRef) is native `LLVMBuildIndirectBr`
	on [buildInvoke: (BuilderRef), (ValueRef), (Array[ValueRef]), (Size_t), (BasicBlockRef), (BasicBlockRef), (Str)] (ValueRef) is native `LLVMBuildInvoke`
	on [buildInvoke2: (BuilderRef), (TypeRef), (ValueRef), (Array[ValueRef]), (Size_t), (BasicBlockRef), (BasicBlockRef), (Str)] (ValueRef) is native `LLVMBuildInvoke2`
	on [buildUnreachable: (BuilderRef)] (ValueRef) is native `LLVMBuildUnreachable`
	on [buildResume: (BuilderRef), (ValueRef)] (ValueRef) is native `LLVMBuildResume`
	on [buildLandingPad: (BuilderRef), (TypeRef), (ValueRef), (Size_t), (Str)] (ValueRef) is native `LLVMBuildLandingPad`
	on [buildCleanupRet: (BuilderRef), (ValueRef), (BasicBlockRef)] (ValueRef) is native `LLVMBuildCleanupRet`
	on [buildCatchRet: (BuilderRef), (ValueRef), (BasicBlockRef)] (ValueRef) is native `LLVMBuildCatchRet`
	on [buildCatchPad: (BuilderRef), (ValueRef), (Array[ValueRef]), (Size_t), (Str)] (ValueRef) is native `LLVMBuildCatchPad`
	on [buildCleanupPad: (BuilderRef), (ValueRef), (Array[ValueRef]), (Size_t), (Str)] (ValueRef) is native `LLVMBuildCleanupPad`
	on [buildCatchSwitch: (BuilderRef), (ValueRef), (BasicBlockRef), (Size_t), (Str)] (ValueRef) is native `LLVMBuildCatchSwitch`
	on [addCase: (ValueRef), (ValueRef), (BasicBlockRef)] is native `LLVMAddCase`
	on [addDestination: (ValueRef), (BasicBlockRef)] is native `LLVMAddDestination`
	on [getNumClauses: (ValueRef)] (Size_t) is native `LLVMGetNumClauses`
	on [getClause: (ValueRef), (Size_t)] (ValueRef) is native `LLVMGetClause`
	on [addClause: (ValueRef), (ValueRef)] is native `LLVMAddClause`
	on [isCleanup: (ValueRef)] (Bool) is native `LLVMIsCleanup`
	on [setCleanup: (ValueRef), (Bool)] is native `LLVMSetCleanup`
	on [addHandler: (ValueRef), (BasicBlockRef)] is native `LLVMAddHandler`
	on [getNumHandlers: (ValueRef)] (Size_t) is native `LLVMGetNumHandlers`
	on [getHandlers: (ValueRef), (Array[BasicBlockRef])] is native `LLVMGetHandlers`
	on [getArgOperand: (ValueRef), (Size_t)] (ValueRef) is native `LLVMGetArgOperand`
	on [setArgOperand: (ValueRef), (Size_t), (ValueRef)] is native `LLVMSetArgOperand`
	on [getParentCatchSwitch: (ValueRef)] (ValueRef) is native `LLVMGetParentCatchSwitch`
	on [setParentCatchSwitch: (ValueRef), (ValueRef)] is native `LLVMSetParentCatchSwitch`
	on [buildAdd: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildAdd`
	on [buildNSWAdd: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildNSWAdd`
	on [buildNUWAdd: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildNUWAdd`
	on [buildFAdd: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildFAdd`
	on [buildSub: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildSub`
	on [buildNSWSub: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildNSWSub`
	on [buildNUWSub: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildNUWSub`
	on [buildFSub: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildFSub`
	on [buildMul: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildMul`
	on [buildNSWMul: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildNSWMul`
	on [buildNUWMul: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildNUWMul`
	on [buildFMul: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildFMul`
	on [buildUDiv: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildUDiv`
	on [buildExactUDiv: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildExactUDiv`
	on [buildSDiv: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildSDiv`
	on [buildExactSDiv: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildExactSDiv`
	on [buildFDiv: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildFDiv`
	on [buildURem: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildURem`
	on [buildSRem: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildSRem`
	on [buildFRem: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildFRem`
	on [buildShl: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildShl`
	on [buildLShr: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildLShr`
	on [buildAShr: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildAShr`
	on [buildAnd: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildAnd`
	on [buildOr: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildOr`
	on [buildXor: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildXor`
	on [buildBinOp: (BuilderRef), (Opcode), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildBinOp`
	on [buildNeg: (BuilderRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildNeg`
	on [buildNSWNeg: (BuilderRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildNSWNeg`
	on [buildNUWNeg: (BuilderRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildNUWNeg`
	on [buildFNeg: (BuilderRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildFNeg`
	on [buildNot: (BuilderRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildNot`
	on [buildMalloc: (BuilderRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildMalloc`
	on [buildArrayMalloc: (BuilderRef), (TypeRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildArrayMalloc`
	on [buildMemSet: (BuilderRef), (ValueRef), (ValueRef), (ValueRef), (Size_t)] (ValueRef) is native `LLVMBuildMemSet`
	on [buildMemCpy: (BuilderRef), (ValueRef), (Size_t), (ValueRef), (Size_t), (ValueRef)] (ValueRef) is native `LLVMBuildMemCpy`
	on [buildMemMove: (BuilderRef), (ValueRef), (Size_t), (ValueRef), (Size_t), (ValueRef)] (ValueRef) is native `LLVMBuildMemMove`
	on [buildAlloca: (BuilderRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildAlloca`
	on [buildArrayAlloca: (BuilderRef), (TypeRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildArrayAlloca`
	on [buildFree: (BuilderRef), (ValueRef)] (ValueRef) is native `LLVMBuildFree`
	on [buildLoad: (BuilderRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildLoad`
	on [buildLoad2: (BuilderRef), (TypeRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildLoad2`
	on [buildStore: (BuilderRef), (ValueRef), (ValueRef)] (ValueRef) is native `LLVMBuildStore`
	on [buildGEP: (BuilderRef), (ValueRef), (Array[ValueRef]), (Size_t), (Str)] (ValueRef) is native `LLVMBuildGEP`
	on [buildInBoundsGEP: (BuilderRef), (ValueRef), (Array[ValueRef]), (Size_t), (Str)] (ValueRef) is native `LLVMBuildInBoundsGEP`
	on [buildStructGEP: (BuilderRef), (ValueRef), (Size_t), (Str)] (ValueRef) is native `LLVMBuildStructGEP`
	on [buildGEP2: (BuilderRef), (TypeRef), (ValueRef), (Array[ValueRef]), (Size_t), (Str)] (ValueRef) is native `LLVMBuildGEP2`
	on [buildInBoundsGEP2: (BuilderRef), (TypeRef), (ValueRef), (Array[ValueRef]), (Size_t), (Str)] (ValueRef) is native `LLVMBuildInBoundsGEP2`
	on [buildStructGEP2: (BuilderRef), (TypeRef), (ValueRef), (Size_t), (Str)] (ValueRef) is native `LLVMBuildStructGEP2`
	on [buildGlobalString: (BuilderRef), (Str), (Str)] (ValueRef) is native `LLVMBuildGlobalString`
	on [buildGlobalStringPtr: (BuilderRef), (Str), (Str)] (ValueRef) is native `LLVMBuildGlobalStringPtr`
	on [getVolatile: (ValueRef)] (Bool) is native `LLVMGetVolatile`
	on [setVolatile: (ValueRef), (Bool)] is native `LLVMSetVolatile`
	on [getOrdering: (ValueRef)] (AtomicOrdering) is native `LLVMGetOrdering`
	on [setOrdering: (ValueRef), (AtomicOrdering)] is native `LLVMSetOrdering`
	on [buildTrunc: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildTrunc`
	on [buildZExt: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildZExt`
	on [buildSExt: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildSExt`
	on [buildFPToUI: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildFPToUI`
	on [buildFPToSI: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildFPToSI`
	on [buildUIToFP: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildUIToFP`
	on [buildSIToFP: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildSIToFP`
	on [buildFPTrunc: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildFPTrunc`
	on [buildFPExt: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildFPExt`
	on [buildPtrToInt: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildPtrToInt`
	on [buildIntToPtr: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildIntToPtr`
	on [buildBitCast: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildBitCast`
	on [buildAddrSpaceCast: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildAddrSpaceCast`
	on [buildZExtOrBitCast: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildZExtOrBitCast`
	on [buildSExtOrBitCast: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildSExtOrBitCast`
	on [buildTruncOrBitCast: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildTruncOrBitCast`
	on [buildCast: (BuilderRef), (Opcode), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildCast`
	on [buildPointerCast: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildPointerCast`
	on [buildIntCast2: (BuilderRef), (ValueRef), (TypeRef), (Bool), (Str)] (ValueRef) is native `LLVMBuildIntCast2`
	on [buildFPCast: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildFPCast`
	on [buildIntCast: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildIntCast`
	on [buildICmp: (BuilderRef), (IntPredicate), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildICmp`
	on [buildFCmp: (BuilderRef), (RealPredicate), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildFCmp`
	on [buildPhi: (BuilderRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildPhi`
	on [buildCall: (BuilderRef), (ValueRef), (Array[ValueRef]), (Size_t), (Str)] (ValueRef) is native `LLVMBuildCall`
	on [buildCall2: (BuilderRef), (TypeRef), (ValueRef), (Array[ValueRef]), (Size_t), (Str)] (ValueRef) is native `LLVMBuildCall2`
	on [buildSelect: (BuilderRef), (ValueRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildSelect`
	on [buildVAArg: (BuilderRef), (ValueRef), (TypeRef), (Str)] (ValueRef) is native `LLVMBuildVAArg`
	on [buildExtractElement: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildExtractElement`
	on [buildInsertElement: (BuilderRef), (ValueRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildInsertElement`
	on [buildShuffleVector: (BuilderRef), (ValueRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildShuffleVector`
	on [buildExtractValue: (BuilderRef), (ValueRef), (Size_t), (Str)] (ValueRef) is native `LLVMBuildExtractValue`
	on [buildInsertValue: (BuilderRef), (ValueRef), (ValueRef), (Size_t), (Str)] (ValueRef) is native `LLVMBuildInsertValue`
	on [buildIsNull: (BuilderRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildIsNull`
	on [buildIsNotNull: (BuilderRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildIsNotNull`
	on [buildPtrDiff: (BuilderRef), (ValueRef), (ValueRef), (Str)] (ValueRef) is native `LLVMBuildPtrDiff`
	on [buildFence: (BuilderRef), (AtomicOrdering), (Bool), (Str)] (ValueRef) is native `LLVMBuildFence`
	on [buildAtomicRMW: (BuilderRef), (AtomicRMWBinOp), (ValueRef), (ValueRef), (AtomicOrdering), (Bool)] (ValueRef) is native `LLVMBuildAtomicRMW`
	on [buildAtomicCmpXchg: (BuilderRef), (ValueRef), (ValueRef), (ValueRef), (AtomicOrdering), (AtomicOrdering), (Bool)] (ValueRef) is native `LLVMBuildAtomicCmpXchg`
	on [isAtomicSingleThread: (ValueRef)] (Bool) is native `LLVMIsAtomicSingleThread`
	on [setAtomicSingleThread: (ValueRef), (Bool)] is native `LLVMSetAtomicSingleThread`
	on [getCmpXchgSuccessOrdering: (ValueRef)] (AtomicOrdering) is native `LLVMGetCmpXchgSuccessOrdering`
	on [setCmpXchgSuccessOrdering: (ValueRef), (AtomicOrdering)] is native `LLVMSetCmpXchgSuccessOrdering`
	on [getCmpXchgFailureOrdering: (ValueRef)] (AtomicOrdering) is native `LLVMGetCmpXchgFailureOrdering`
	on [setCmpXchgFailureOrdering: (ValueRef), (AtomicOrdering)] is native `LLVMSetCmpXchgFailureOrdering`	
	
	;-- Module Providers
	on [createModuleProviderForExistingModule: (ModuleRef)] (ModuleProviderRef) is native `LLVMCreateModuleProviderForExistingModule`
	on [disposeModuleProvider: (ModuleProviderRef)] is native `LLVMDisposeModuleProvider`

	;-- Pass Registry
	on [getGlobalPassRegistry] (PassRegistryRef) is native `LLVMGetGlobalPassRegistry`

	;-- Pass Managers
	on [createPassManager] (PassManagerRef) is native `LLVMCreatePassManager`
	on [createFunctionPassManagerForModule: (ModuleRef)] (PassManagerRef) is native `LLVMCreateFunctionPassManagerForModule`
	on [createFunctionPassManager: (ModuleProviderRef)] (PassManagerRef) is native `LLVMCreateFunctionPassManager`
	on [runPassManager: (PassManagerRef), (ModuleRef)] (Bool) is native `LLVMRunPassManager`
	on [initializeFunctionPassManager: (PassManagerRef)] (Bool) is native `LLVMInitializeFunctionPassManager`
	on [runFunctionPassManager: (PassManagerRef), (ValueRef)] (Bool) is native `LLVMRunFunctionPassManager`
	on [finalizeFunctionPassManager: (PassManagerRef)] (Bool) is native `LLVMFinalizeFunctionPassManager`
	on [disposePassManager: (PassManagerRef)] is native `LLVMDisposePassManager`

	;-- Threading
	on [startMultithreaded] (Bool) is native `LLVMStartMultithreaded`
	on [stopMultithreaded] is native `LLVMStopMultithreaded`
	on [isMultithreaded] (Bool) is native `LLVMIsMultithreaded`

	;-- Disassembler
	on [createDisasm: (Str), (Ptr[Void]), (Int32), (OpInfoCallback), (SymbolLookupCallback)] (Ptr[Void]) is native `LLVMCreateDisasm`
	on [createDisasmCPU: (Str), (Str), (Ptr[Void]), (Int32), (OpInfoCallback), (SymbolLookupCallback)] (Ptr[Void]) is native `LLVMCreateDisasmCPU`
	on [createDisasmCPUFeatures: (Str), (Str), (Str), (Ptr[Void]), (Int32), (OpInfoCallback), (SymbolLookupCallback)] (Ptr[Void]) is native `LLVMCreateDisasmCPUFeatures`
	on [setDisasmOptions: (Ptr[Void]), (DisasmOptions)] (Bool) is native `LLVMSetDisasmOptions`
	on [disasmDispose: (Ptr[Void])] is native `LLVMDisasmDispose`
	on [disasmInstruction: (Ptr[Void]), (Array[UInt8]), (UInt64), (UInt64), (Str), (Size_t)] (Size_t) is native `LLVMDisasmInstruction`
	
	;-- Execution Engine
	on [linkInMCJIT] is native `LLVMLinkInMCJIT`
	on [linkInInterpreter] is native `LLVMLinkInInterpreter`
	on [createGenericValueOfInt: (TypeRef), (UInt64), (Bool)] (GenericValueRef) is native `LLVMCreateGenericValueOfInt`
	on [createGenericValueOfPointer: (Ptr[Void])] (GenericValueRef) is native `LLVMCreateGenericValueOfPointer`
	on [createGenericValueOfFloat: (TypeRef), (Dec64)] (GenericValueRef) is native `LLVMCreateGenericValueOfFloat`
	on [genericValueIntWidth: (GenericValueRef)] (Size_t) is native `LLVMGenericValueIntWidth`
	on [genericValueToInt: (GenericValueRef), (Bool)] (UInt64) is native `LLVMGenericValueToInt`
	on [genericValueToPointer: (GenericValueRef)] (Ptr[Void]) is native `LLVMGenericValueToPointer`
	on [genericValueToFloat: (TypeRef), (GenericValueRef)] (Dec64) is native `LLVMGenericValueToFloat`
	on [disposeGenericValue: (GenericValueRef)] is native `LLVMDisposeGenericValue`
	on [createExecutionEngineForModule: (Ptr[ExecutionEngineRef]), (ModuleRef), (Ptr[Str])] (Bool) is native `LLVMCreateExecutionEngineForModule`
	on [createInterpreterForModule: (Ptr[ExecutionEngineRef]), (ModuleRef), (Ptr[Str])] (Bool) is native `LLVMCreateInterpreterForModule`
	on [createJITCompilerForModule: (Ptr[ExecutionEngineRef]), (ModuleRef), (Size_t), (Ptr[Str])] (Bool) is native `LLVMCreateJITCompilerForModule`
	on [initializeMCJITCompilerOptions: (Ptr[MCJITCompilerOptions]), (Size_t)] is native `LLVMInitializeMCJITCompilerOptions`
	on [createMCJITCompilerForModule: (Ptr[ExecutionEngineRef]), (ModuleRef), (Ptr[MCJITCompilerOptions]), (Size_t), (Ptr[Str])] (Bool) is native `LLVMCreateMCJITCompilerForModule`
	on [disposeExecutionEngine: (ExecutionEngineRef)] is native `LLVMDisposeExecutionEngine`
	on [runStaticConstructors: (ExecutionEngineRef)] is native `LLVMRunStaticConstructors`
	on [runStaticDestructors: (ExecutionEngineRef)] is native `LLVMRunStaticDestructors`
	on [runFunctionAsMain: (ExecutionEngineRef), (ValueRef), (Size_t), (Array[Str]), (Array[Str])] (Int32) is native `LLVMRunFunctionAsMain`
	on [runFunction: (ExecutionEngineRef), (ValueRef), (Size_t), (Array[GenericValueRef])] (GenericValueRef) is native `LLVMRunFunction`
	on [freeMachineCodeForFunction: (ExecutionEngineRef), (ValueRef)] is native `LLVMFreeMachineCodeForFunction`
	on [addModule: (ExecutionEngineRef), (ModuleRef)] is native `LLVMAddModule`
	on [removeModule: (ExecutionEngineRef), (ModuleRef), (Ptr[ModuleRef]), (Ptr[Str])] (Bool) is native `LLVMRemoveModule`
	on [findFunction: (ExecutionEngineRef), (Str), (Ptr[ValueRef])] (Bool) is native `LLVMFindFunction`
	on [recompileAndRelinkFunction: (ExecutionEngineRef), (ValueRef)] (Ptr[Void]) is native `LLVMRecompileAndRelinkFunction`
	on [getExecutionEngineTargetData: (ExecutionEngineRef)] (TargetDataRef) is native `LLVMGetExecutionEngineTargetData`
	on [getExecutionEngineTargetMachine: (ExecutionEngineRef)] (TargetMachineRef) is native `LLVMGetExecutionEngineTargetMachine`
	on [addGlobalMapping: (ExecutionEngineRef), (ValueRef), (Ptr[Void])] is native `LLVMAddGlobalMapping`
	on [getPointerToGlobal: (ExecutionEngineRef), (ValueRef)] (Ptr[Void]) is native `LLVMGetPointerToGlobal`
	on [getGlobalValueAddress: (ExecutionEngineRef), (Str)] (UInt64) is native `LLVMGetGlobalValueAddress`
	on [getFunctionAddress: (ExecutionEngineRef), (Str)] (UInt64) is native `LLVMGetFunctionAddress`
	on [createSimpleMCJITMemoryManager: (Ptr[Void]), (MemoryManagerAllocateCodeSectionCallback), (MemoryManagerAllocateDataSectionCallback), (MemoryManagerFinalizeMemoryCallback), (MemoryManagerDestroyCallback)] (MCJITMemoryManagerRef) is native `LLVMCreateSimpleMCJITMemoryManager`
	on [disposeMCJITMemoryManager: (MCJITMemoryManagerRef)] is native `LLVMDisposeMCJITMemoryManager`
	on [createGDBRegistrationListener] (JITEventListenerRef) is native `LLVMCreateGDBRegistrationListener`
	on [createIntelJITEventListener] (JITEventListenerRef) is native `LLVMCreateIntelJITEventListener`
	on [createOProfileJITEventListener] (JITEventListenerRef) is native `LLVMCreateOProfileJITEventListener`
	on [createPerfJITEventListener] (JITEventListenerRef) is native `LLVMCreatePerfJITEventListener`
		
	;-- Initialization Routines
	on [initializeTransformUtils: (PassRegistryRef)] is native `LLVMInitializeTransformUtils`
	on [initializeScalarOpts: (PassRegistryRef)] is native `LLVMInitializeScalarOpts`
	on [initializeObjCARCOpts: (PassRegistryRef)] is native `LLVMInitializeObjCARCOpts`
	on [initializeVectorization: (PassRegistryRef)] is native `LLVMInitializeVectorization`
	on [initializeInstCombine: (PassRegistryRef)] is native `LLVMInitializeInstCombine`
	on [initializeAggressiveInstCombiner: (PassRegistryRef)] is native `LLVMInitializeAggressiveInstCombiner`
	on [initializeIPO: (PassRegistryRef)] is native `LLVMInitializeIPO`
	on [initializeInstrumentation: (PassRegistryRef)] is native `LLVMInitializeInstrumentation`
	on [initializeAnalysis: (PassRegistryRef)] is native `LLVMInitializeAnalysis`
	on [initializeIPA: (PassRegistryRef)] is native `LLVMInitializeIPA`
	on [initializeCodeGen: (PassRegistryRef)] is native `LLVMInitializeCodeGen`
	on [initializeTarget: (PassRegistryRef)] is native `LLVMInitializeTarget`
	
	;-- Object File reading & writing
	on [createBinary: (MemoryBufferRef), (ContextRef), (Ptr[Str])] (BinaryRef) is native `LLVMCreateBinary`
	on [disposeBinary: (BinaryRef)] is native `LLVMDisposeBinary`
	on [binaryCopyMemoryBuffer: (BinaryRef)] (MemoryBufferRef) is native `LLVMBinaryCopyMemoryBuffer`
	on [binaryGetType: (BinaryRef)] (BinaryType) is native `LLVMBinaryGetType`
	on [machOUniversalBinaryCopyObjectForArch: (BinaryRef), (Str), (Size_t), (Ptr[Str])] (BinaryRef) is native `LLVMMachOUniversalBinaryCopyObjectForArch`
	on [objectFileCopySectionIterator: (BinaryRef)] (SectionIteratorRef) is native `LLVMObjectFileCopySectionIterator`
	on [objectFileIsSectionIteratorAtEnd: (BinaryRef), (SectionIteratorRef)] (Bool) is native `LLVMObjectFileIsSectionIteratorAtEnd`
	on [objectFileCopySymbolIterator: (BinaryRef)] (SymbolIteratorRef) is native `LLVMObjectFileCopySymbolIterator`
	on [objectFileIsSymbolIteratorAtEnd: (BinaryRef), (SymbolIteratorRef)] (Bool) is native `LLVMObjectFileIsSymbolIteratorAtEnd`
	on [disposeSectionIterator: (SectionIteratorRef)] is native `LLVMDisposeSectionIterator`
	on [moveToNextSection: (SectionIteratorRef)] is native `LLVMMoveToNextSection`
	on [moveToContainingSection: (SectionIteratorRef), (SymbolIteratorRef)] is native `LLVMMoveToContainingSection`
	on [disposeSymbolIterator: (SymbolIteratorRef)] is native `LLVMDisposeSymbolIterator`
	on [moveToNextSymbol: (SymbolIteratorRef)] is native `LLVMMoveToNextSymbol`
	on [getSectionName: (SectionIteratorRef)] (Str) is native `LLVMGetSectionName`
	on [getSectionSize: (SectionIteratorRef)] (UInt64) is native `LLVMGetSectionSize`
	on [getSectionContents: (SectionIteratorRef)] (Str) is native `LLVMGetSectionContents`
	on [getSectionAddress: (SectionIteratorRef)] (UInt64) is native `LLVMGetSectionAddress`
	on [getSectionContainsSymbol: (SectionIteratorRef), (SymbolIteratorRef)] (Int32) is native `LLVMGetSectionContainsSymbol`
	on [getRelocations: (SectionIteratorRef)] (RelocationIteratorRef) is native `LLVMGetRelocations`
	on [disposeRelocationIterator: (RelocationIteratorRef)] is native `LLVMDisposeRelocationIterator`
	on [isRelocationIteratorAtEnd: (SectionIteratorRef), (RelocationIteratorRef)] (Int32) is native `LLVMIsRelocationIteratorAtEnd`
	on [moveToNextRelocation: (RelocationIteratorRef)] is native `LLVMMoveToNextRelocation`
	on [getSymbolName: (SymbolIteratorRef)] (Str) is native `LLVMGetSymbolName`
	on [getSymbolAddress: (SymbolIteratorRef)] (UInt64) is native `LLVMGetSymbolAddress`
	on [getSymbolSize: (SymbolIteratorRef)] (UInt64) is native `LLVMGetSymbolSize`
	on [getRelocationOffset: (RelocationIteratorRef)] (UInt64) is native `LLVMGetRelocationOffset`
	on [getRelocationSymbol: (RelocationIteratorRef)] (SymbolIteratorRef) is native `LLVMGetRelocationSymbol`
	on [getRelocationType: (RelocationIteratorRef)] (UInt64) is native `LLVMGetRelocationType`
	on [getRelocationTypeName: (RelocationIteratorRef)] (Str) is native `LLVMGetRelocationTypeName`
	on [getRelocationValueString: (RelocationIteratorRef)] (Str) is native `LLVMGetRelocationValueString`
	on [createObjectFile: (MemoryBufferRef)] (ObjectFileRef) is native `LLVMCreateObjectFile`
	on [disposeObjectFile: (ObjectFileRef)] is native `LLVMDisposeObjectFile`
	on [getSections: (ObjectFileRef)] (SectionIteratorRef) is native `LLVMGetSections`
	on [isSectionIteratorAtEnd: (ObjectFileRef), (SectionIteratorRef)] (Bool) is native `LLVMIsSectionIteratorAtEnd`
	on [getSymbols: (ObjectFileRef)] (SymbolIteratorRef) is native `LLVMGetSymbols`
	on [isSymbolIteratorAtEnd: (ObjectFileRef), (SymbolIteratorRef)] (Bool) is native `LLVMIsSymbolIteratorAtEnd`
	
	;-- Remarks
	on [remarkStringGetData: (RemarkStringRef)] (Str) is native `LLVMRemarkStringGetData`
	on [remarkStringGetLen: (RemarkStringRef)] (UInt32) is native `LLVMRemarkStringGetLen`
	on [remarkDebugLocGetSourceFilePath: (RemarkDebugLocRef)] (RemarkStringRef) is native `LLVMRemarkDebugLocGetSourceFilePath`
	on [remarkDebugLocGetSourceLine: (RemarkDebugLocRef)] (UInt32) is native `LLVMRemarkDebugLocGetSourceLine`
	on [remarkDebugLocGetSourceColumn: (RemarkDebugLocRef)] (UInt32) is native `LLVMRemarkDebugLocGetSourceColumn`
	on [remarkArgGetKey: (RemarkArgRef)] (RemarkStringRef) is native `LLVMRemarkArgGetKey`
	on [remarkArgGetValue: (RemarkArgRef)] (RemarkStringRef) is native `LLVMRemarkArgGetValue`
	on [remarkArgGetDebugLoc: (RemarkArgRef)] (RemarkDebugLocRef) is native `LLVMRemarkArgGetDebugLoc`
	on [remarkEntryDispose: (RemarkEntryRef)] is native `LLVMRemarkEntryDispose`
	on [remarkEntryGetPassName: (RemarkEntryRef)] (RemarkStringRef) is native `LLVMRemarkEntryGetPassName`
	on [remarkEntryGetRemarkName: (RemarkEntryRef)] (RemarkStringRef) is native `LLVMRemarkEntryGetRemarkName`
	on [remarkEntryGetFunctionName: (RemarkEntryRef)] (RemarkStringRef) is native `LLVMRemarkEntryGetFunctionName`
	on [remarkEntryGetDebugLoc: (RemarkEntryRef)] (RemarkDebugLocRef) is native `LLVMRemarkEntryGetDebugLoc`
	on [remarkEntryGetHotness: (RemarkEntryRef)] (UInt64) is native `LLVMRemarkEntryGetHotness`
	on [remarkEntryGetNumArgs: (RemarkEntryRef)] (UInt32) is native `LLVMRemarkEntryGetNumArgs`
	on [remarkEntryGetFirstArg: (RemarkEntryRef)] (RemarkArgRef) is native `LLVMRemarkEntryGetFirstArg`
	on [remarkEntryGetNextArg: (RemarkArgRef), (RemarkEntryRef)] (RemarkArgRef) is native `LLVMRemarkEntryGetNextArg`
	on [remarkParserCreateYAML: (Ptr[Void]), (UInt64)] (RemarkParserRef) is native `LLVMRemarkParserCreateYAML`
	on [remarkParserGetNext: (RemarkParserRef)] (RemarkEntryRef) is native `LLVMRemarkParserGetNext`
	on [remarkParserHasError: (RemarkParserRef)] (Bool) is native `LLVMRemarkParserHasError`
	on [remarkParserGetErrorMessage: (RemarkParserRef)] (Str) is native `LLVMRemarkParserGetErrorMessage`
	on [remarkParserDispose: (RemarkParserRef)] is native `LLVMRemarkParserDispose`
	on [remarkVersion] (UInt32) is native `LLVMRemarkVersion`
	
	;-- Target information
	on [initializeAllTargetInfos] is native `LLVMInitializeAllTargetInfos`
	on [initializeAllTargets] is native `LLVMInitializeAllTargets`
	on [initializeAllTargetMCs] is native `LLVMInitializeAllTargetMCs`
	on [initializeAllAsmPrinters] is native `LLVMInitializeAllAsmPrinters`
	on [initializeAllAsmParsers] is native `LLVMInitializeAllAsmParsers`
	on [initializeAllDisassemblers] is native `LLVMInitializeAllDisassemblers`
	on [initializeNativeTarget] (Bool) is native `LLVMInitializeNativeTarget`
	on [initializeNativeAsmParser] (Bool) is native `LLVMInitializeNativeAsmParser`
	on [initializeNativeAsmPrinter] (Bool) is native `LLVMInitializeNativeAsmPrinter`
	on [initializeNativeDisassembler] (Bool) is native `LLVMInitializeNativeDisassembler`
	on [getModuleDataLayout: (ModuleRef)] (TargetDataRef) is native `LLVMGetModuleDataLayout`
	on [setModuleDataLayout: (ModuleRef), (TargetDataRef)] is native `LLVMSetModuleDataLayout`
	on [createTargetData: (Str)] (TargetDataRef) is native `LLVMCreateTargetData`
	on [disposeTargetData: (TargetDataRef)] is native `LLVMDisposeTargetData`
	on [addTargetLibraryInfo: (TargetLibraryInfoRef), (PassManagerRef)] is native `LLVMAddTargetLibraryInfo`
	on [copyStringRepOfTargetData: (TargetDataRef)] (Str) is native `LLVMCopyStringRepOfTargetData`
	on [byteOrder: (LLVMTargetDataRef)] (ByteOrdering) is native `LLVMByteOrder`
	on [pointerSize: (TargetDataRef)] (Size_t) is native `LLVMPointerSize`
	on [pointerSizeForAS: (TargetDataRef), (Size_t)] (Size_t) is native `LLVMPointerSizeForAS`
	on [intPtrType: (TargetDataRef)] (TypeRef) is native `LLVMIntPtrType`
	on [intPtrTypeForAS: (TargetDataRef), (Size_t)] (TypeRef) is native `LLVMIntPtrTypeForAS`
	on [intPtrTypeInContext: (ContextRef), (TargetDataRef)] (TypeRef) is native `LLVMIntPtrTypeInContext`
	on [intPtrTypeForASInContext: (ContextRef), (TargetDataRef), (Size_t)] (TypeRef) is native `LLVMIntPtrTypeForASInContext`
	on [abiAlignmentOfType: (TargetDataRef), (TypeRef)] (Size_t) is native `LLVMABIAlignmentOfType`
	on [callFrameAlignmentOfType: (TargetDataRef), (TypeRef)] (Size_t) is native `LLVMCallFrameAlignmentOfType`
	on [preferredAlignmentOfType: (TargetDataRef), (TypeRef)] (Size_t) is native `LLVMPreferredAlignmentOfType`
	on [preferredAlignmentOfGlobal: (TargetDataRef), (ValueRef)] (Size_t) is native `LLVMPreferredAlignmentOfGlobal`
	on [elementAtOffset: (TargetDataRef), (TypeRef), (UInt64)] (Size_t) is native `LLVMElementAtOffset`
	
	;-- LTO 
	module LTO {
		kind SymbolAttributes (Int32) {
			has alignmentMask           => 0x0000001F
			has permissionsMask         => 0x000000E0
			has permissionsCode         => 0x000000A0
			has permissionsData         => 0x000000C0
			has permissionsRodata       => 0x00000080
			has definitionMask          => 0x00000700
			has definitionRegular       => 0x00000100
			has definitionTentative     => 0x00000200
			has definitionWeak          => 0x00000300
			has definitionUndefined     => 0x00000400
			has definitionWeakundef     => 0x00000500
			has scopeMask               => 0x00003800
			has scopeInternal           => 0x00000800
			has scopeHidden             => 0x00001000
			has scopeProtected          => 0x00002000
			has scopeDefault            => 0x00001800
			has scopeDefaultCanBeHidden => 0x00002800
			has comdat                  => 0x00004000
			has alias                   => 0x00008000
		}
		
		kind DebugModel (Int32) {
			has none  => 0
			has dwarf => 1
		}

		kind CodeGenModel (Int32) {
			has static       => 0
			has dynamic      => 1
			has dynamicNoPIC => 2
			has default      => 3
		}

		kind CodeGenDiagnosticSeverity (Int32) {
			has error   => 0
			has warning => 1
			has remark  => 3
			has note    => 2
		}
		
		kind Status (Int32) {
			has unknown            => 0
			has optSuccess         => 1
			has readSuccess        => 2
			has readDailure        => 3
			has writeDailure       => 4
			has noTarget           => 5
			has noWork             => 6
			has moduleMergeFailure => 7
			has asmFailure         => 8
			has nullObject         => 9
		}
		
		alias Module {
			on [create: (Str)] (Module) is static is native `lto_module_create`
			on [createFromMemory: (Ptr[Void]), (Size_t)] (Module) is static is native `lto_module_create_from_memory`
			on [createFromMemoryWithPath: (Ptr[Void]), (Size_t), (Str)] (Module) is static is native `lto_module_create_from_memory_with_path`
			on [createInLocalContext: (Ptr[Void]), (Size_t), (Str)] (Module) is static is native `lto_module_create_in_local_context`
			on [createInCodegenContext: (Ptr[Void]), (Size_t), (Str), (CodeGen)] (Module) is static is native `lto_module_create_in_codegen_context`
			on [createFromFD: (Int32), (Str), (Size_t)] (Module) is static is native `lto_module_create_from_fd`
			on [createFromFDAtOffset: (Int32), (Str), (Size_t), (Size_t), (UInt32)] (Module) is static is native `lto_module_create_from_fd_at_offset`
			
			on [dispose] is native `lto_module_dispose`
			on [getTargetTriple] (Str) is native `lto_module_get_target_triple`
			on [setTargetTriple: (Str)] is native `lto_module_set_target_triple`
			on [getSymbolName: (UInt32)] (Str) is native `lto_module_get_symbol_name`
			on [getSymbolAttribute: (UInt32)] (SymbolAttributes) is native `lto_module_get_symbol_attribute`
			on [getLinkeropts] (Str) is native `lto_module_get_linkeropts`
		}

		alias CodeGen {
			on [create] (CodeGen) is static is native `lto_codegen_create`
			on [createInLocalContext] (CodeGen) is static is native `lto_codegen_create_in_local_context`

			on [setDiagnosticHandler: (DiagnosticHandler), (Ptr[Void])] is native `lto_codegen_set_diagnostic_handler`
			on [dispose] is native `lto_codegen_dispose`
			on [addModule: (Module)] (Bool) is native `lto_codegen_add_module`
			on [setModule: (Module)] is native `lto_codegen_set_module`
			on [setDebugModel: (DebugModel)] (Bool) is native `lto_codegen_set_debug_model`
			on [setPICModel: (CodeGenModel)] (Bool) is native `lto_codegen_set_pic_model`
			on [setCPU: (Str)] is native `lto_codegen_set_cpu`
			on [setAssemblerPath: (Str)] is native `lto_codegen_set_assembler_path`
			on [setAssemblerArgs: (Array[Str]), (Int32)] is native `lto_codegen_set_assembler_args`
			on [addMustPreserveSymbol: (Str)] is native `lto_codegen_add_must_preserve_symbol`
			on [writeMergedModules: (Str)] (Bool) is native `lto_codegen_write_merged_modules`
			on [compile: (Ptr[Size_t])] (Ptr[Void]) is native `lto_codegen_compile`
			on [compileToFile: (Array[Str])] (Bool) is native `lto_codegen_compile_to_file`
			on [optimize] (Bool) is native `lto_codegen_optimize`
			on [compileOptimized: (Ptr[Size_t])] (Ptr[Void]) is native `lto_codegen_compile_optimized`
			on [debugOptions: (Str)] is native `lto_codegen_debug_options`

			on [setShouldInternalize: (Bool)] is native `lto_codegen_set_should_internalize`
			on [setShouldEmbedUselists: (Bool)] is native `lto_codegen_set_should_embed_uselists`
		}
		
		alias Input {
			on [create: (Ptr[Void]), (Size_t), (Str)] (Input) is static is native `lto_input_create`

			on [dispose] is native `lto_input_dispose`
			on [getNumDependentLibraries] (Size_t) is native `lto_input_get_num_dependent_libraries`
			on [getDependentLibrary: (Size_t), (Ptr[Size_t])] (Str) is native `lto_input_get_dependent_library`
		}

		alias DiagnosticHandler = Func[Void, CodeGenDiagnosticSeverity, Str, Ptr[Void]]
		
		;-- Link Time Optimizer
		alias LTO_t {
			on [createOptimizer] (LTO_t) is static is native `llvm_create_optimizer`
			
			on [destroyOptimizer] is native `llvm_destroy_optimizer`
			on [readObjectFile: (Str)] (Status) is native `llvm_read_object_file`
			on [optimizeModules: (Str)] (Status) is native `llvm_optimize_modules`
		}
		
		;-- LTO 
		on [getVersion] (Str) is native `lto_get_version`
		on [getErrorMessage] (Str) is native `lto_get_error_message`
		on [moduleIsObjectFile: (Str)] (Bool) is native `lto_module_is_object_file`
		on [moduleIsObjectFileForTarget: (Str), (Str)] (Bool) is native `lto_module_is_object_file_for_target`
		on [moduleHasObjcCategory: (Ptr[Void]), (Size_t)] (Bool) is native `lto_module_has_objc_category`
		on [moduleIsObjectFileInMemory: (Ptr[Void]), (Size_t)] (Bool) is native `lto_module_is_object_file_in_memory`
		on [moduleIsObjectFileInMemoryForTarget: (Ptr[Void]), (Size_t), (Str)] (Bool) is native `lto_module_is_object_file_in_memory_for_target`

		on [initializeDisassembler] is native `lto_initialize_disassembler`
		
		on [inputCreate: (Ptr[Void]), (Size_t), (Str)] (Input) is native `lto_input_create`
		on [inputDispose: (Input)] is native `lto_input_dispose`
		on [inputGetNumDependentLibraries: (Input)] (Size_t) is native `lto_input_get_num_dependent_libraries`
		on [inputGetDependentLibrary: (Input), (Size_t), (Ptr[Size_t])] (Str) is native `lto_input_get_dependent_library`
		
		;-- Thin LTO 
		module Thin {
			alias CodeGen {
				on [create] (CodeGen) is static is native `thinlto_create_codegen`

				on [dispose] is native `thinlto_codegen_dispose`
				on [addModule: (Str), (Str), (Int32)] is native `thinlto_codegen_add_module`
				on [process] is native `thinlto_codegen_process`
				on [moduleGetObject: (UInt32)] (ObjectBuffer) is native `thinlto_module_get_object`
				on [moduleGetObjectFile: (UInt32)] (Str) is native `thinlto_module_get_object_file`
				on [setPICModel: (CodeGenModel)] (Bool) is native `thinlto_codegen_set_pic_model`
				on [setSavetempsDir: (Str)] is native `thinlto_codegen_set_savetemps_dir`
				on [setGeneratedObjectsDir: (Str)] is native `thinlto_set_generated_objects_dir`
				on [setCPU: (Str)] is native `thinlto_codegen_set_cpu`
				on [disableCodegen: (Bool)] is native `thinlto_codegen_disable_codegen`
				on [setCodegenOnly: (Bool)] is native `thinlto_codegen_set_codegen_only`

				on [addMustPreserveSymbol: (Str), (Int32)] is native `thinlto_codegen_add_must_preserve_symbol`
				on [addCrossReferencedSymbol: (Str), (Int32)] is native `thinlto_codegen_add_cross_referenced_symbol`

				on [setCacheDir: (Str)] is native `thinlto_codegen_set_cache_dir`
				on [setCachePruningInterval: (Int32)] is native `thinlto_codegen_set_cache_pruning_interval`
				on [setFinalCacheSizeRelativeToAvailableSpace: (Size_t)] is native `thinlto_codegen_set_final_cache_size_relative_to_available_space`
				on [setCacheEntryExpiration: (Size_t)] is native `thinlto_codegen_set_cache_entry_expiration`
				on [setCacheSizeBytes: (Size_t)] is native `thinlto_codegen_set_cache_size_bytes`
				on [setCacheSizeMegabytes: (Size_t)] is native `thinlto_codegen_set_cache_size_megabytes`
				on [setCacheSizeFiles: (Size_t)] is native `thinlto_codegen_set_cache_size_files`
			}
			
			class ObjectBuffer {
				my size (Size_t)
				my buffer (Str)
			}
			
			;-- ThinLTO
			on [debugOptions: (Array[Str]), (Int32)] is native `thinlto_debug_options`
			on [moduleIsThinLTO: (Module)] (Bool) is native `lto_module_is_thinlto`
		}
	}
}