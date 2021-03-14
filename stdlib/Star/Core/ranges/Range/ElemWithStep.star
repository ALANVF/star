type S of Step
type T of Elem {
	operator `+` [other (S)] (T)
	operator `-` [other (S)] (T)
}
alias ElemWithStep[S] = T