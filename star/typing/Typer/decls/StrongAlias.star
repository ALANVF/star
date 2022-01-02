class StrongAlias of RealAlias {
	my staticMembers (Array[Member]) is getter = #[]
	my staticMethods (Array[StaticMethod]) is getter = #[]
	my members (Array[Member]) is getter = #[]
	my methods (Array[Method]) is getter = #[]
	my operators (Array[Operator]) is getter = #[]
	my noInherit (Bool) = false
}