use Core

category Power for Num {
	on [reciprocal] (This) {
		return 1 / this
	}

	on [sign: (This)] (This) {
		return this[abs] * sign[sign]
	}

	on [percent] (Dec) {
		return this / 100
	}

	;-- Maybe add this:
	; https://github.com/crystal-lang/crystal/blob/master/src/number.cr#L210
}