module Main {
    on [main] {
        if 1 ?= 1 {
            Core[say: true]
        }
        
        if 1 ?= 2 {
            Core[say: true]
        } else {
            Core[say: false]
        }
        
        if 1 ?= 2 {
            Core[say: "1 equals 2"]
        } orif 1 != 1 {
            Core[say: "1 doesn't equal 1"]
        } else {
            Core[say: "ya both wrong"]
        }
        
        ; there is also a shorthand for an if/else statement
        (Core.stdin[IO[Int] prompt] >= 0)[yes: "positive" no: "negative"]
        
        ; as a replacement for a long if/orif/else statement
        case {
            at false && true => Core[say: 1]
            at false ^^ true => Core[say: 2]
            at false || true => Core[say: 3]
            at false !! true => Core[say: 4]
            else             => Core[say: 5]
        } ;=> 2
        
        ;[conditional operators:
            `&&` = and
            `||` = or
            `^^` = xor
			`!!` = nor
            `!`  = not
            `?`  = boolean coercion
        ]
    }
}
