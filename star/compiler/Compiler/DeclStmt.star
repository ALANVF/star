kind DeclStmt {
	has [typeDecl: (TypeDecl)]
	has [anonEnum: (Enum.Anon)]
	has [anonUnion: (Union.Anon)]
	has [namespace: (Namespace)]
	has [using: (Using)]
	has [includeLib: (Str)]
	has [includeFile: (Str)]
	has [member: (Member)]
	has [memberList: (MemberList)]
	has [method: (AnyMethod)]
	
	on [form: (Int) = 0] (Str) {
		match this {
			at This[typeDecl: my decl] => return decl[:form]
			at This[anonEnum: my enum] => return enum[:form] + #";"
			at This[anonUnion: my union] => return union[:form] + #";"
			at This[namespace: my namespace] => return namespace[:form]
			at This[using: my using] => return using[form]
			at This[includeLib: my path] => return "#include <\(path[Expr escape])>"
			at This[includeFile: my path] => return "#include \"\(path[Expr escape])\""
			at This[member: my member] => return member[:form]
			at This[memberList: my memberList] => return memberList[:form]
			at This[method: my method] => return method[:form]
		}
	}
}