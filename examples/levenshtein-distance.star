module Main {
	on [levenshteinDistanceWith: str1 (Str) and: str2 (Str)] (Int) {
		my empty = #[0] * str2.length
		my mat = #[0[to: str2.length]] + 1[to: str2.length][collect: #[$.0] + empty]

		for my i, my char1 in: str1 {
			for my j, my char2 in: str2 {
				mat[at: i + 1][at: j + 1] = {
					if char1 ?= char2 {
						return mat[at: i][at: j]
					} else {
						return #[
							mat[at: i][at: j]
							mat[at: i][at: j + 1]
							mat[at: i + 1][at: j]
						][min] + 1
					}
				}
			}
		}

		return mat[at: -1][at: -1]
	}

	on [main] {
		Core[say: Main[levenshteinDistanceWith: "kitten" and: "sitting"]] ;=> 3
	}
}