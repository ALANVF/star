type Ret
protocol Func[Ret] of Func[Ret, _], Func[Ret, _, _], Func[Ret, _, _, _] {
	on [call] (Ret)

	on [call: (_)] (Ret) is inline {
		return this[call]
	}
	
	on [call: (_), (_)] (Ret) is inline {
		return this[call]
	}
	
	on [call: (_), (_), (_)] (Ret) is inline {
		return this[call]
	}
}

type Ret
type Arg1 if Arg1 != Void
protocol Func[Ret, Arg1] of Func[Ret, Arg1, _], Func[Ret, Arg1, _, _] {
	on [call: (Arg1)] (Ret)

	on [call: arg1 (Arg1), (_)] (Ret) is inline {
		return this[call: arg1]
	}
	
	on [call: arg1 (Arg1), (_), (_)] (Ret) is inline {
		return this[call: arg1]
	}
}

type Ret
type Arg1 if Arg1 != Void
type Arg2 if Arg2 != Void
protocol Func[Ret, Arg1, Arg2] of Func[Ret, Arg1, Arg2, _] {
	on [call: (Arg1), (Arg2)] (Ret)

	on [call: arg1 (Arg1), arg2 (Arg2), (_)] (Ret) is inline {
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