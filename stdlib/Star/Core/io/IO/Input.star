use Native

protocol Input of Closeable {
	;== Checking

	on [size] (Int) is getter

	on [available] (Int) is getter

	on [eof] (Bool) is getter


	;== Reading

	;on [read] (UInt8)

	;on [maybeRead] (Maybe[UInt8])

	;on [read: length (Int)] (Array[UInt8])

	category IO[Int8] { on [read] (Int8) }
	category IO[UInt8] { on [read] (UInt8) }
	category IO[Int16] { on [read] (Int16) }
	category IO[UInt16] { on [read] (UInt16) }
	category IO[Int32] { on [read] (Int32) }
	category IO[UInt32] { on [read] (UInt32) }
	category IO[Int64] { on [read] (Int64) }
	category IO[UInt64] { on [read] (UInt64) }
	category IO[Float32] { on [read] (Float32) }
	category IO[Float64] { on [read] (Float64) }
	
	category IO[Int] {
		on [read] (Int)
	}

	category IO[Dec] {
		on [read] (Dec)
	}

	category IO[Bool] {
		on [read] (Bool)
	}

	category IO[Char] {
		on [read] (Char)
	}

	category IO[Str] {
		on [readLine] (Str)
		
		on [readAll] (Str)

		on [read: length (Int)] (Str)
		
		on [readCStr] (Str)
	}

	type T of Readable
	category IO[T] {
		on [read] (T) {
			return T[readFrom: this]
		}
	}
}