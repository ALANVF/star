package reporting;

import hx.strings.ansi.AnsiColor;

@:publicFields
@:structInit
class Severity {
	static final NOTE: Severity = {description: "note", color: GREEN};
	static final WARNING: Severity = {description: "warning", color: YELLOW};
	static final ERROR: Severity = {description: "error", color: RED};
	static final INTERNAL_ERROR: Severity = {description: "internal compiler error", color: MAGENTA};

	final description: String;
	final color: AnsiColor;
}