namespace Symbolics

open System

type Expression = 
    | Const of int
    | Var of string
    | Add of Expression * Expression
    | Sub of Expression * Expression
    | Mul of Expression * Expression  
    | Div of Expression * Expression  
    | Exp of Expression * Expression
    | Neg of Expression


module Parsing = 
    
    /// Divide a string into variables and operators
    module Tokenizing = 
        
        let symbols = "+-*/^()" |> Set.ofSeq

        let tokenize str = 
            let rec loop buf chrs = seq {
                match chrs with
                | c :: rest when symbols.Contains(c) ->
                    if buf |> List.isEmpty |> not then
                        yield new String(buf |> Array.ofList |> Array.rev)
                    yield string c
                    yield! loop [] rest

                | c :: rest when Char.IsWhiteSpace(c) -> 
                    yield! loop buf rest

                | c :: rest ->
                    yield! loop (c::buf) rest

                | [] -> 
                    if buf |> List.isEmpty |> not then
                        yield new String(buf |> Array.ofList |> Array.rev)
            }
            loop [] (str |> List.ofSeq)
