use Native

; pretending that LibC doesn't already exist
module LibC is native `c` {
	on [strdup: (Str)] (Str) is native
	on [free: (Ptr[Void])] is native
}

module Main {
	on [main] {
		my charPtr = LibC[strdup: #c_str "banana"]
		Core[say: charPtr[Core.Str]]
		LibC[free: charPtr[Native.Ptr[Void]]]
	}
}
