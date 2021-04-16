package compiler;

class Class extends ClassLike {
	function form(indent = 0) {
		return _form("class ", indent);
	}
}