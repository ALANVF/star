module Main is main {
	on [main] is main {
		my res = 0
		recurse my i = 0 {
			if i ?= 5 {
				res = i
				break
			} else {
				next with: i + 1
			}
		}
		Core[say: res]
	}
}