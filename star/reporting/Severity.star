class Severity {
	my note           is static is getter = This[description: "note" color: Color.green]
	my warning        is static is getter = This[description: "warning" color: Color.yellow]
	my error          is static is getter = This[description: "error" color: Color.red]
	my internal_error is static is getter = This[description: "internal compiler error" color: Color.magenta]
	
	my description (Str) is getter
	my color (Color) is getter
}