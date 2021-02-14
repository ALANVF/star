module Opaque {
	alias A

	alias B is hidden {
		on [c] (B) {
			return this
		}
	}
}