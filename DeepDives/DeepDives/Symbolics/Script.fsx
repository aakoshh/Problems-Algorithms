// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Expressions.fs"
open Symbolics.Parsing.Tokenizing

// Define your library scripting code here

tokenize "1 + x + (x'*3)^2 " |> List.ofSeq