package errors;

@:using(errors.Access)
enum Access {
	Static;
	Instance;
}


inline function desc(self: Access) return self._match(
	at(Static) => "Type",
	at(Instance) => "Value of type"
);