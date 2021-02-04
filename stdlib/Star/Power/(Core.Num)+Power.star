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
	; https://github.com/crystal-lang/crystal/blob/f0901e1c825de1095d77998ffe8890bfca66def0/src/number.cr#L370
}