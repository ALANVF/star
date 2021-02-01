class BF {
	my table = #[0] * 100
	my lstack = #[]
	my ibuffer = #[]
	my cell = 0
	my ptr = 0
	
	on [run: code (Str)] {
		while ptr < code.length {
			match code[at: ptr] {
				at #">" {
					cell++
					ptr++
				}
				
				at #"<" {
					cell--
					ptr++
				}
				
				at #"+" {
					table[at: cell]++
					ptr++
				}
				
				at #"-" {
					table[at: cell]--
					ptr++
				}
				
				at #"." {
					Core.stdout[write: table[at: cell][Char]]
					ptr++
				}
				
				at #"," {
					table[at: cell] = Core.stdin[IO[Char] read][Int]
					ptr++
				}
				
				at #"[" {
					if table[at: cell] ?= 0 {
						my i = 1
						
						while i > 0 {
							ptr++
							my j = code[at: ptr]
							
							match j {
								at #"[" => i++
								at #"]" => i--
							}
						}
					} else {
						ptr++
					}
				}
				
				at #"]" {
					if table[at: cell] != 0 {
						my i = 1
						
						while i > 0 {
							ptr--
							my j = code[at: ptr]
							
							match j {
								at #"[" => i--
								at #"]" => i++
							}
						}
					} else {
						ptr++
					}
				}

				else => ptr++
			}
		}
	}
}

module Main {
	on [main] {
		BF[new][run: "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."] ;=> "Hello World!"
	}
}
