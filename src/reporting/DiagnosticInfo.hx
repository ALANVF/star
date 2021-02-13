package reporting;

import text.Span;

@:struct
@:structInit
@:publicFields
class SpannedInfo {
	var span: Span;
	var message: Null<String> = null;
	var isPrimary: Bool = false;
	var isSecondary: Bool = false;
}

@:using(DiagnosticInfo.Tools)
enum DiagnosticInfo {
	Spanned(info: SpannedInfo);
	Footnote(info: {message: String});
}

@:publicFields
@:noCompletion
class Tools {
	static function getSpannedInfo(info: DiagnosticInfo) return switch info {
		case Spanned(i): i;
		default: null;
	};

	static function getSecondaryInfo(info: DiagnosticInfo) return switch info {
		case Spanned(i) if(i.isSecondary): i;
		default: null;
	};

	static function getPrimaryInfo(info: DiagnosticInfo) return switch info {
		case Spanned(i) if(i.isPrimary): i;
		default: null;
	};

	static function getFootnote(info: DiagnosticInfo) return switch info {
		case Footnote(i): i;
		default: null;
	};
}