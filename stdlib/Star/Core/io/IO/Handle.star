use Native

class Handle is native[repr: `voidptr`] {
	on [open: path (Str) mode: (Int)] (This) is static is native `io_open`

	on [readBool] (Bool) is native `io_read_bool`
	on [readInt8] (Int8) is native `io_read_i8`
	on [readUInt8] (UInt8) is native `io_read_u8`
	on [readInt16] (Int16) is native `io_read_i16`
	on [readUInt16] (UInt16) is native `io_read_u16`
	on [readInt32] (Int32) is native `io_read_i32`
	on [readUInt32] (UInt32) is native `io_read_u32`
	on [readInt64] (Int64) is native `io_read_i64`
	on [readUInt64] (UInt64) is native `io_read_u64`
	on [readDec] (Dec) is native `io_read_d64`
	on [readFloat32] (Float32) is native `io_read_f32`
	on [readFloat64] (Float64) is native `io_read_f64`
	on [readStr: length (Int32)] (Str) is native `io_read_str`
	on [readLineStr] (Str) is native `io_read_line_str`
	on [readFullStr] (Str) is native `io_read_full_str`
	on [readCStr] (Str) is native `io_read_cstr`
	on [readPtr: length (Int32) out: realLength (Ptr[Int32])] (Ptr[UInt8]) is native `io_read_ptr`

	on [writeBool: (Bool)] is native `io_write_bool`
	on [writeInt8: (Int8)] is native `io_write_i8`
	on [writeUInt8: (UInt8)] is native `io_write_u8`
	on [writeInt16: (Int16)] is native `io_write_i16`
	on [writeUInt16: (UInt16)] is native `io_write_u16`
	on [writeInt32: (Int32)] is native `io_write_i32`
	on [writeUInt32: (UInt32)] is native `io_write_u32`
	on [writeInt64: (Int64)] is native `io_write_i64`
	on [writeUInt64: (UInt64)] is native `io_write_u64`
	on [writeDec: (Dec)] is native `io_write_d64`
	on [writeFloat32: (Float32)] is native `io_write_f32`
	on [writeFloat64: (Float64)] is native `io_write_f64`
	on [writeStr: (Str)] is native `io_write_str`
	on [writePtr: (Ptr[UInt8]) length: (Int32)] is native `io_write_ptr`

	on [seek: offset (Int64) from: (Int)] is native `io_seek`

	on [tell] (Int64) is native `io_tell`

	on [size] (Int64) is native `io_size`

	on [eof] (Bool) is native `io_eof`

	on [flush] is native `io_flush`

	on [close] is native `io_close`
}