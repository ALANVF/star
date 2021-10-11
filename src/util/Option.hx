package util;

import haxe.macro.Expr;

enum Option<T> {
	None;
	Some(value: T);
}

@:publicFields
class OptionHelper {
	static inline function fromNull<T>(t: Enum<Option<T>>, value: Null<T>) {
		return if(value == null) {
			None;
		} else {
			Some((value : T));
		}
	}

	static inline function toNull<T>(opt: Option<T>) {
		return switch opt {
			case None: null;
			case Some(v): v;
		}
	}

	static inline function contains<T>(opt: Option<T>, value: T) {
		return switch opt {
			case None: false;
			case Some(v): v == value;
		}
	}

	static inline function map<T, U>(opt: Option<T>, fn: T -> U) {
		return switch opt {
			case None: None;
			case Some(v): Some(fn(v));
		}
	}

	static inline function flatMap<T, U>(opt: Option<T>, fn: T -> Option<U>) {
		return switch opt {
			case None: None;
			case Some(v): fn(v);
		}
	}

	static inline function filter<T>(opt: Option<T>, fn: T -> Bool) {
		return switch opt {
			case Some(v) if(fn(v)): Some(v);
			case _: None;
		}
	}

	static inline function exists<T>(opt: Option<T>, fn: T -> Bool) {
		return switch opt {
			case Some(v): fn(v);
			case _: false;
		}
	}
	
	static inline function forEach<T>(opt: Option<T>, fn: T -> Void): Void {
		switch opt {
			case Some(v): fn(v);
			case None:
		}
	}

	static inline function value<T>(opt: Option<T>) {
		return switch opt {
			case Some(v): v;
			case None: throw "Value was empty!";
		}
	}

	static inline function orElse<T, U: T, V: T>(opt: Option<U>, other: V) {
		return switch opt {
			case Some(v): v;
			case None: other;
		};
	}

	static macro function orElseDo<T, U>(value: ExprOf<Option<T>>, or: ExprOf<U>): ExprOf<U> {

		return macro switch($value) {
			case Some(__anon__Some): __anon__Some;
			case None: $or;
		};
	}

	static macro function orDo<T, U>(value: ExprOf<Option<T>>, or: ExprOf<U>): ExprOf<Option<U>> {

		return macro switch($value) {
			case __anon__Some = Some(_): __anon__Some;
			case None: $or;
		};
	}
	
	static macro function doOrElse<T, U>(value: ExprOf<Option<T>>, and, or: ExprOf<U>): ExprOf<U> {
		switch and { case macro $i{n} => $v:
			var dv = switch v {
				case {expr: EDisplay(v2, _)}: v2;
				default: v;
			};
			return macro switch($value) {
				case Some($i{n}): $dv;
				case None: $or;
			};
			
		default: throw "error!"; }
	}

	static inline function isNone<T>(opt: Option<T>) {
		return opt.getIndex() == 0;
	}
	
	static inline function isSome<T>(opt: Option<T>) {
		return opt.getIndex() == 1;
	}
}