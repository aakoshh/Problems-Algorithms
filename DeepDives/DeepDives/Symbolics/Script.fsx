
#load "Expressions.fs"

Symbolics.Parsing.parse "1 * 2 * 3"
Symbolics.Parsing.parse "1 * (2 * 3)"
Symbolics.Parsing.parse "(1 * 2) * 3"
Symbolics.Parsing.parse "1 / 2 / 3"
Symbolics.Parsing.parse "3 * x^2 * y"
Symbolics.Parsing.parse "3 * x^(2+a)"
Symbolics.Parsing.parse "-x^2"
Symbolics.Parsing.parse "-x^-2^y"
Symbolics.Parsing.parse "x---x"
Symbolics.Parsing.parse "1 / x^2"

Symbolics.Parsing.parse "-a^2 + 2*b^-((c+1)/2) - c - 2"

open Symbolics.Parsing
open Symbolics.Printing

"2*x^2 + 3*(x+y) + 4" |> parse |> toString
"3 * x^(2+a)" |> parse |> toString