
#load "Expressions.fs"
open Symbolics.Parsing
open Symbolics.Printing
open Symbolics.Transformations

parse "1 * 2 * 3"
parse "1 * (2 * 3)"
parse "(1 * 2) * 3"
parse "1 / 2 / 3"
parse "3 * x^2 * y"
parse "3 * x^(2+a)"
parse "-x^2"
parse "-x^-2^y"
parse "x---x"
parse "1 / x^2"

parse "-a^2 + 2*b^-((c+1)/2) - c - 2"



"2*x^2 + 3*(x+y) + 4" |> parse |> toString
"3 * x^(2+a)" |> parse |> toString



"1 + a + 2 + a" |> parse |> simplify |> toString
"a + 2 * a" |> parse |> simplify |> toString
