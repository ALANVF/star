package compiler;

class Struct extends ClassLike {
	function form(indent = 0) {
		return _form("struct ", indent);
	}
}