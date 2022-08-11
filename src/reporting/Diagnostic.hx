package reporting;

@:publicFields
class Diagnostic {
	var severity: Null<Severity> = null;
	var code: Null<String> = null;
	var message: Null<String> = null;
	var info: Array<DiagnosticInfo>;

	function new(a: {
		?severity: Severity,
		?code: String,
		?message: String,
		info: Array<DiagnosticInfo>
	}) {
		severity = a.severity;
		code = a.code;
		message = a.message;
		info = a.info;
	}
}