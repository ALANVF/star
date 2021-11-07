module Unsafe {}

category Unsafe for Value {
	type T of Value
	on [T] is native `value_unsafe_cast`
}