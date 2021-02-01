module Native {}

category Native for Value {
	on [address] (UInt64) is native `value_address`
}