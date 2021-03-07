type Ret
type Arg1 if Arg1 != Void
type Arg2 if Arg2 != Void
type Arg3 if Arg3 != Void
protocol Func[Ret] of Func[Ret, Arg1], Func[Ret, Arg1, Arg2], Func[Ret, Arg1, Arg2, Arg3] {
	on [call] (Ret)

	on [call: (Arg1)] (Ret) is inline {
		return this[call]
	}
	
	on [call: (Arg1), (Arg2)] (Ret) is inline {
		return this[call]
	}
	
	on [call: (Arg1), (Arg2), (Arg3)] (Ret) is inline {
		return this[call]
	}
}

type Ret
type Arg1 if Arg1 != Void
type Arg2 if Arg2 != Void
type Arg3 if Arg3 != Void
protocol Func[Ret, Arg1] of Func[Ret, Arg1, Arg2], Func[Ret, Arg1, Arg2, Arg3] {
	on [call: (Arg1)] (Ret)

	on [call: arg1 (Arg1), (Arg2)] (Ret) is inline {
		return this[call: arg1]
	}
	
	on [call: arg1 (Arg1), (Arg2), (Arg3)] (Ret) is inline {
		return this[call: arg1]
	}
}

type Ret
type Arg1 if Arg1 != Void
type Arg2 if Arg2 != Void
type Arg3 if Arg3 != Void
protocol Func[Ret, Arg1, Arg2] of Func[Ret, Arg1, Arg2, Arg3] {
	on [call: (Arg1), (Arg2)] (Ret)

	on [call: arg1 (Arg1), arg2 (Arg2), (Arg3)] (Ret) is inline {
		return this[call: arg1, arg2]
	}
}

type Ret
type Arg1 if Arg1 != Void
type Arg2 if Arg2 != Void
type Arg3 if Arg3 != Void
protocol Func[Ret, Arg1, Arg2, Arg3] {
	on [call: (Arg1), (Arg2), (Arg3)] (Ret)
}

;... etc