protocol Ordered of Comparable {
	on [next] (This) is inline => return ++(this)
	on [previous] (This) is inline => return --(this)

	operator `++` (This)
	operator `--` (This)
}