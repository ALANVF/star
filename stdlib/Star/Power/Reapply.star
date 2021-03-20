type T[_]
type A
alias Reapply[T[_], A] = T[A]

type T[_, _]
type A
type B
alias Reapply[T[_, _], A, B] = T[A, B]

type T[_, _, _]
type A
type B
type C
alias Reapply[T[_, _, _], A, B, C] = T[A, B, C]

;... and so on