package compiler;

@:using(compiler.Body.BodyTools)
enum Body {
	BBlock(b: Block);
	BPure;
	BDefault;
	BDisable;
}


@:publicFields
class BodyTools {
	static function form(body: Body, indent = 0) return switch body {
		case BBlock(b): " " + b.form(indent);
		case BPure: " = 0;";
		case BDefault: " = default;";
		case BDisable: " = delete;";
	}
}