package compiler;

abstract class NamedMethod extends AnyMethod {
	var template: Option<Template> = None;
	var name: String;
}