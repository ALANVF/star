package compiler;

class Union extends ClassLike {
	function form(indent = 0) {
		return _form("union ", indent);
	}
}