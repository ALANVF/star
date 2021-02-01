package reporting.render;

interface IDiagnosticRenderer {
	function render(diagnostic: Diagnostic): Void;
}