
#load "Expressions.fs"

Symbolics.Parsing.parse "1 * 2 * 3"
Symbolics.Parsing.parse "1 * (2 * 3)"
Symbolics.Parsing.parse "(1 * 2) * 3"
Symbolics.Parsing.parse "1 / 2 / 3"
Symbolics.Parsing.parse "3 * x^2 * y"
Symbolics.Parsing.parse "3 * x^(2+a)"
Symbolics.Parsing.parse "-x^2"
Symbolics.Parsing.parse "-x^-2^y"

Symbolics.Parsing.parse "-a^2 + 2*b^-((c+1)/2) - c - 2"
