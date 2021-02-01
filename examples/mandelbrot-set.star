module Main {
	on [mandelbrot: a (Complex)] (Complex) {
		return [#[a] * 50 with: 0[Complex] reduce: $0 ** 2 + $1]
	}

	on [main] {
		for my y from: 1.0 downto: -1.0 by: -0.05 {
			for my x from: -2.0 to: 0.5 by: 0.0315 {
				my c = Complex[real: x imag: y]
				
				Core[say: [Main[mandelbrot: c][abs] < 2 yes: "*" no: " "] end: ""]
			}

			Core[say]
		}
	}
}