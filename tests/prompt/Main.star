module Main is main {
	on [main] is main {
		my name = Core[prompt: "What is your name? "]
		Core[say: "Hello, \(name)!"]
	}
}