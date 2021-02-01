type Ret
protocol Func[Ret] {
	on [call] (Ret)
}

type Ret
type Arg1
protocol Func[Ret, Arg1] {
	on [call: (Arg1)] (Ret)
}

type Ret
type Arg1
type Arg2
protocol Func[Ret, Arg1, Arg2] {
	on [call: (Arg1), (Arg2)] (Ret)
}

type Ret
type Arg1
type Arg2
type Arg3
protocol Func[Ret, Arg1, Arg2, Arg3] {
	on [call: (Arg1), (Arg2), (Arg3)] (Ret)
}

;... etc