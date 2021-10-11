category Unsafe for Str {
	on [at: index (Int) set: str (Str)] is setter {
		match str.length {
			at 0 {
				this[after: index moveBy: -1]
				length--
			}
			at 1 => buffer[at: index] = str[Unsafe at: index]
			at my strLength {
				this[resizeBy: strLength]
				this[after: index moveBy: strLength]
				str[copyInto: buffer + index]
				length += strLength - 1
			}
		}
	}
}