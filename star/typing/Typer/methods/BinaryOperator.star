class BinaryOperator of Operator {
	kind Op (Str) {
		has plus => "+"
		has minus => "-"
		has times => "*"
		has pow => "**"
		has div => "/"
		has intDiv => "//"
		has mod => "%"
		has isMod => "%%"
		has bitAnd => "&"
		has bitOr => "|"
		has bitXor => "^"
		has shl => "<<"
		has shr => ">>"
		has eq => "?="
		has ne => "!="
		has gt => ">"
		has ge => ">="
		has lt => "<"
		has le => "<="
		has and => "&&"
		has or => "||"
		has xor => "^^"
		has nor => "!!"
	}

	alias Ops = Array[Op]

	
}