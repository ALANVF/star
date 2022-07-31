package codegen;

import haxe.Int64;


@:forward
abstract Dec64(Int64) from Int64 {
	public static final NaN: Dec64 = Int64.parseString("-9223372036854775680");

	// Taken from here: https://github.com/vpisarev/DEC64/blob/alt/dec64_string.c#L356
	public static function fromString(str: String): Dec64 {
		var pos: Int;
		var c: Char;
		var digits = 0;
		var leading = true;
		var ok = false;
		var point = false;

		var coef: Int64 = 0;
		var exp: Int64 = 0;
		var exponent: Int64 = 0;
		var sign: Int64;
		var signExp: Int64 = 0;

		if(str.length == 0) return NaN;

		final bytes = str.toBytes();//@:privateAccess new haxe.io.Bytes(@:privateAccess str.bytes, str.length);

		c = bytes.get(0);

		if(c == '-'.code) {
			c = bytes.get(1);
			pos = 1;
			sign = -1;
		} else {
			pos = 0;
			sign = 1;
		}

		while(pos < bytes.length) {
			c._match(
				at('0'.code) => {
					ok = true;
					if(leading) {
						if(point) exponent--;
					} else {
						digits++;
						if(digits > 18) {
							if(!point) exponent++;
						} else {
							coef *= 10;
							if(point) exponent--;
						}
					}
				},
				at('1'.code ... '9'.code) => {
					ok = true;
					leading = false;
					digits++;
					if(digits > 18) {
						if(!point) exponent++;
					} else {
						coef = (coef * 10) + (c - '0'.code);
						if(point) exponent--;
					}
				},
				at('.'.code) => {
					if(point) return NaN;
					point = true;
				},
				at('e'.code) => {
					if(ok) {
						ok = false;
						exp = 0;
						signExp = 1;
						pos++;
						c = bytes.get(pos);

						c._match(
							at('-'.code) => {
								signExp = -1;
								pos++;
								c = bytes.get(pos);
							},
							at('+'.code) => {
								pos++;
								c = bytes.get(pos);
							},
							_ => {}
						);

						while(pos < bytes.length) {
							c._match(
								at('0'.code ... '9'.code) => {
									ok = true;
									exp = (exp * 10) + (c - '0'.code);
									if(exp < 0) return NaN;
								},
								_ => {
									return NaN;
								}
							);

							pos++;
							c = bytes.get(pos);
						}
					}

					//if(-128 <= exponent && exponent <= 127) {
						return ((sign * coef) << 8)
								| (((signExp * exp) + exponent) & 255);
					//} else {
					//	throw "out of range!";
					//}
				},
				_ => {
					return NaN;
				}
			);

			pos++;
			c = bytes.get(pos);
		}
		
		//if(-128 <= exponent && exponent <= 127) {
			return ((sign * coef) << 8) | (exponent & 255);
		//} else {
		//	throw "out of range!";
		//}
	}
}