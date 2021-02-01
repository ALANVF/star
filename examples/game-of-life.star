class Cell {
	my x (Int)
	my y (Int)
	
	on [neighbors] (Array[Cell]) {
		return #[-1, 0, 1][crossWith: #[-1, 0, 1] andCollect: Cell[x: x + $.0 y: y + $.1]]
	}
}

class Colony {
	my width (Int)
	my height (Int)
	my cells (Array[Cell])
	
	on [neighborCounts] (Bag[Cell]) is hidden {
		return Bag[Cell][new: cells[collectAll: $0[neighbors]]]
	}
	
	on [runGeneration] is hidden {
		cells = this[neighborCounts][keepIf: {|cell, count|
			return (count ?= 2 && cells[contains: cell]) || count ?= 3
		}].keys
	}

	on [runTimes: times (Int)] {
		for my i from: 1 to: times {
			Core
			-> [say: "Generation \(i):"]
			-> [say: this]
			-> [say]
			
			this[runGeneration]
		}
	}

	on [Str] {
		my out = ""

		for my y from: 0 upto: height {
			for my x from: 0 upto: width {
				out[add: cells[contains: Cell[:x :y]][yes: "# " no: "- "]]
			}

			out[add: "\n"]
		}

		return out[trim]
	}
}

module Main {
	on [main] {
		my blinker = #[
			Cell[x: 1 y: 0]
			Cell[x: 1 y: 1]
			Cell[x: 1 y: 2]
		]
		
		my colony = Colony[width: 3 height: 3 cells: blinker]
		
		Core[say: "Blinker:"]
		colony[runTimes: 3]
		
		my glider = #[
			Cell[x: 1 y: 0]
			Cell[x: 2 y: 1]
			Cell[x: 0 y: 2]
			Cell[x: 1 y: 2]
			Cell[x: 2 y: 2]
		]
		
		colony = Colony[width: 8 height: 8 cells: glider]

		Core[say: "Glider:"]
		colony[runTimes: 20]
	}
}