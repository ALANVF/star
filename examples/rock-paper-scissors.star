kind Choice {
	has rock
	has paper
	has scissors

	on [weakness] (Choice) {
		match this {
			at Choice.rock => return Choice.paper
			at Choice.paper => return Choice.scissors
			at Choice.scissors => return Choice.rock
		}
	}

	on [name] (Str) {
		match this {
			at Choice.rock => return "rock"
			at Choice.paper => return "paper"
			at Choice.scissors => return "scissors"
		}
	}
}

module Main {
	on [main] {
		my choices = Bag #(
			Choice.rock => 0
			Choice.paper => 0
			Choice.scissors => 0
		)

		while true {
			my userChoice = {
				my input = Core.stdin[
					prompt: "Rock, paper or scissors? "
					filter: $0[trim][lowercase]
					choices: #[
						"r", "rock"
						"p", "paper"
						"s", "scissors"
						"q", "quit"
					]
				]

				match input {
					at "r" || "rock" => return Choice.rock
					at "p" || "paper" => return Choice.paper
					at "s" || "scissors" => return Choice.scissors
					at "q" || "quit" => break
				}
			}

			my cpuChoice = choices[max][weakness]

			if userChoice ?= cpuChoice[weakness] {
				Core[say: "\(userChoice[name][capitalize]) beats \(cpuChoice[name]), so you win!"]
			} orif cpuChoice ?= userChoice[weakness] {
				Core[say: "\(cpuChoice[name][capitalize]) beats \(userChoice[name]), so you lose..."]
			} else {
				Core[say: "It's a draw"]
			}

			choices[at: userChoice]++
		}

		Core[say: "Ok bye"]
	}
}