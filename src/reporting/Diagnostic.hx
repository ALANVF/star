package reporting;

@:publicFields
class Diagnostic {
	var severity: Option<Severity> = None;
	var code: Option<String> = None;
	var message: Option<String> = None;
	var info: Array<DiagnosticInfo> = [];

	function new(a: {
		?severity: Severity,
		?code: String,
		?message: String,
		info: Array<DiagnosticInfo>
	}) {
		severity = Option.fromNull(a.severity);
		code = Option.fromNull(a.code);
		message = Option.fromNull(a.message);
		info = a.info != null ? a.info : [];
	}
}