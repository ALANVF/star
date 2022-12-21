use Native

protocol Output of Closeable, Flushable {
	;== Writing

	;on [write: (UInt8)]

	;on [write: (Array[UInt8])]

	category IO[Int8] { on [write: value (Int8)] }
	category IO[UInt8] { on [write: value (UInt8)] }
	category IO[Int16] { on [write: value (Int16)] }
	category IO[UInt16] { on [write: value (UInt16)] }
	category IO[Int32] { on [write: value (Int32)] }
	category IO[UInt32] { on [write: value (UInt32)] }
	category IO[Int64] { on [write: value (Int64)] }
	category IO[UInt64] { on [write: value (UInt64)] }
	category IO[Float32] { on [write: value (Float32)] }
	category IO[Float64] { on [write: value (Float64)] }
	
	category IO[Int] {
		on [write: value (Int)]
	}

	category IO[Dec] {
		on [write: value (Dec)]
	}

	category IO[Bool] {
		on [write: value (Bool)]
	}

	category IO[Char] {
		on [write: value (Char)]
	}

	category IO[Str] {
		on [write: value (Str)]

		on [writeRaw: value (Str)]
		
		on [writeCStr: value (Str)]
	}

	type T of Writable
	category IO[T] {
		on [write: value (T)] {
			value[writeTo: this]
		}
	}
}