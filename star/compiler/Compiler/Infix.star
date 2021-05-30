kind Infix {
	has [assign]
	has [assign: op (Infix)]
	has [add]
	has [sub]
	has [mult]
	has [div]
	has [mod]
	has [bitAnd]
	has [bitOr]
	has [bitXor]
	has [shl]
	has [shr]
	has [cmp]
	has [eq]
	has [ne]
	has [gt]
	has [ge]
	has [lt]
	has [le]
	has [and]
	has [or]
	has [comma]
	
	on [form] (Str) {
		match this {
			at This[assign] => return "="
			at This[assign: my op] => return op[form] + "="
			at This[add] => return "+"
			at This[sub] => return "-"
			at This[mult] => return "*"
			at This[div] => return "/"
			at This[mod] => return "%"
			at This[bitAnd] => return "&"
			at This[bitOr] => return "|"
			at This[bitXor] => return "^"
			at This[shl] => return "<<"
			at This[shr] => return ">>"
			at This[cmp] => return "<=>"
			at This[eq] => return "=="
			at This[ne] => return "!="
			at This[gt] => return ">"
			at This[ge] => return ">="
			at This[lt] => return "<"
			at This[le] => return "<="
			at This[and] => return "&&"
			at This[or] => return "||"
			at This[comma] => return ","
		}
	}
}