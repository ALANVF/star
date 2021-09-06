kind Cell {
	has empty => #" "
	has x     => #"X"
	has o     => #"O"

	on [Str] {
		return this.value[Str]
	}
}

kind Winner {
	has player1
	has player2
	has draw
	has none
}

class Board {
	my cells = #[Cell.empty] * 9
	
	on [findWinner] (Winner) {
		my wins = cells[every: 3] + #[
			cells[atEvery: 3]
			cells[atEvery: 4]
			cells[atIndexes: #[2, 4, 6]]
		]
		
		for my line in: wins {
			case {
				at line[all: $0 ?= Cell.player1] => return Winner.player1
				at line[all: $0 ?= Cell.player2] => return Winner.player2
			}
		}
		
		if cells[contains: Cell.empty] {
			return Winner.none
		} else {
			return Winner.draw
		}
	}
	
	on [verifyChoice: index (Int)] (Bool) {
		return 0 < index < 10 && cells[at: index - 1] ?= Cell.empty
	}
	
	on [Str] {
		return cells[collect: $0[joinWith: " | "]][joinWith: "\n--+---+--"]
	}
}

module Main {
	on [main] {
		my board = Board[new]
		my playerTurn = true
		my winner = Winner.none
		
		Core[say: board]
		
		while winner ?= Winner.none {
			if playerTurn {
				Core[say: "Player 1's turn:"]
				
				my choice = Core.stdin[IO[Int]
					prompt: "Choose a cell to play: "
					require: board[verifyChoice: $.0]
				]
				
				board.cells[at: choice - 1] = Cell.player1
			} else {
				Core[say: "Player 2's turn:"]
				
				my choice = 1[to: 9][Array[Int]][keepIf: board[verifyChoice: $.0]][pick]
				
				board.cells[at: choice - 1] = Cell.player2
			}
			
			playerTurn = !playerTurn
			winner = board[findWinner]
			
			Core[say: board]
		}
		
		match winner {
			at Winner.player1 => Core[say: "Player 1 wins!"]
			at Winner.player2 => Core[say: "Player 2 wins!"]
			at Winner.draw => Core[say: "It's a draw!"]
		}
	}
}