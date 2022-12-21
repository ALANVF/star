use Native

;[ TODO
==| - Binary Mode and Text Mode (maybe make them separate types?)
==| - figure out how to obtain file name/path from handle
==| - make Path type that's immutable (and maybe a Str newtype)
]

class File of Input, Output {
	;my path (Str) is getter
	my handle (Handle) is getter
	
	
	on [exists: path (Str)] (Bool) is static

	on [fullPath: path (Str)] (Str) is static


	;== Creating

	init [fromHandle: handle (Handle)] {
		_.handle = handle
	}

	init [open: path (Str) mode: (Mode) = Mode.readWrite] {
		handle = Handle[open: path mode: #kind_id mode]
	}

	init [create: path (Str)] {
		if This[exists: path] {
			throw "error: file already exists"
		} else {
			#init_this This[open: path mode: Mode.write]
		}
	}

	init [read: path (Str)] {
		#init_this This[open: path mode: Mode.read]
	}

	init [write: path (Str)] {
		#init_this This[open: path mode: Mode.write]
	}

	init [append: path (Str)] {
		#init_this This[open: path mode: Mode.append]
	}


	;== Checking

	on [size] (Int) is getter {
		return handle[size][Int]
	}

	on [available] (Int) is getter {
		return [handle[size] - handle[tell] Int]
	}

	on [eof] (Bool) is getter {
		return handle[eof]
	}


	;== Closing

	on [close] {
		handle[close]
	}


	;== Seeking / Telling

	on [seek: offset (Int) from: (SeekFrom)] {
		handle[seek: offset[Int64] from: #kind_id from]
	}

	on [tell] (Int) {
		return handle[tell][Int]
	}


	;== Reading / Writing

	category IO[Int8] {
		on [read] (Int8) {
			return handle[readInt8]
		}

		on [write: value (Int8)] {
			handle[writeInt8: value]
		}
	}

	category IO[UInt8] {
		on [read] (UInt8) {
			return handle[readUInt8]
		}

		on [write: value (UInt8)] {
			handle[writeUInt8: value]
		}
	}

	category IO[Int16] {
		on [read] (Int16) {
			return handle[readInt16]
		}

		on [write: value (Int16)] {
			handle[writeInt16: value]
		}
	}

	category IO[UInt16] {
		on [read] (UInt16) {
			return handle[readUInt16]
		}

		on [write: value (UInt16)] {
			handle[writeUInt16: value]
		}
	}

	category IO[Int32] {
		on [read] (Int32) {
			return handle[readInt32]
		}

		on [write: value (Int32)] {
			handle[writeInt32: value]
		}
	}

	category IO[UInt32] {
		on [read] (UInt32) {
			return handle[readUInt32]
		}

		on [write: value (UInt32)] {
			handle[writeUInt32: value]
		}
	}

	category IO[Int64] {
		on [read] (Int64) {
			return handle[readInt64]
		}

		on [write: value (Int64)] {
			handle[writeInt64: value]
		}
	}

	category IO[UInt64] {
		on [read] (UInt64) {
			return handle[readUInt64]
		}

		on [write: value (UInt64)] {
			handle[writeUInt64: value]
		}
	}

	category IO[Float32] {
		on [read] (Float32) {
			return handle[readFloat32]
		}

		on [write: value (Float32)] {
			handle[writeFloat32: value]
		}
	}

	category IO[Float64] {
		on [read] (Float64) {
			return handle[readFloat64]
		}

		on [write: value (Float64)] {
			handle[writeFloat64: value]
		}
	}
	
	category IO[Int] {
		on [read] (Int) {
			return handle[readInt32][Int]
		}

		on [write: value (Int)] {
			handle[writeInt32: value[Int32]]
		}
	}

	category IO[Dec] {
		on [read] (Dec) {
			return handle[readDec]
		}

		on [write: value (Dec)] {
			handle[writeDec: value]
		}
	}

	category IO[Bool] {
		on [read] (Bool) {
			return handle[readBool]
		}

		on [write: value (Bool)] {
			handle[writeBool: value]
		}
	}

	category IO[Char] {
		on [read] (Char) {
			return handle[readUInt8][Char]
		}

		on [write: value (Char)] {
			handle[writeUInt8: value[UInt8]]
		}
	}

	category IO[Str] {
		on [readLine] (Str) {
			return handle[readLineStr]
		}
		
		on [readAll] (Str) {
			return handle[readFullStr]
		}

		on [read: length (Int)] (Str) {
			return handle[readStr: length]
		}
		
		on [readSized] (Str) {
			return handle[readStr: handle[readInt32]]
		}
		
		on [readCStr] (Str) {
			return handle[readCStr]
		}

		on [write: value (Str)] {
			handle[writeStr: value]
		}

		on [writeSized: value (Str)] {
			handle
			-> [writeInt32: value.length]
			-> [writeStr: value]
		}
		
		on [writeCStr: value (Str)] {
			handle
			-> [writeStr: value]
			-> [writeUInt8: UInt8 0]
		}
	}
}