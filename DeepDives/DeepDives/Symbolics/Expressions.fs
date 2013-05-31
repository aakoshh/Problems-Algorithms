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
    | Neg of Expression // unary minus, might be good for logic later


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

    (*
    Grammar:
    Expr = Prod | Prod + Expr | Expr - Prod
    Prod = Term | Term * Prod | Term / Prod 
    Term = Atom | Atom ^ Term | - Term
    Atom = Const | Var | (Expr) 
    *)

    let (|MulOrDivOp|_|) = function
        | "*" | "/" as op -> Some(op) 
        | _ -> None


    let (|Number|_|) (s: string) = 
        if s |> Seq.forall (fun c -> Char.IsDigit(c)) then Some(Int32.Parse(s)) else None


    let rec (|Expr|) lhs tokens : Expression * string list = 
        match tokens with
        | [] -> 
            failwith "Cannot parse empty sequence"
        | Prod (a, "+" :: (Expr id (e, rest))) ->
            Add(lhs a, e), rest        
        | Prod (a, "-" :: rest) ->
            // subtract needs to be left associative so that x - y - z doesn't become x - (y - z)
            // pass in a function that will first combine the next term with the current left hand side
            // so instead of y + z it will be (x-y) + z
            let lhs' = fun b -> Sub(lhs a,b)
            (|Expr|) lhs' rest
        | Prod (e, rest) -> 
            lhs e, rest
    
    and (|Prod|) tokens = 
        match tokens with
        | Term (a, MulOrDivOp op :: Prod (b, rest)) ->
            let e = match op with
                    | "*" -> Mul(a,b)
                    | "/" -> Div(a,b)
            e, rest
        | Term (e, rest) ->
            e, rest

    // potentially consume two atoms for exponents
    and (|Term|) tokens = 
        match tokens with
        | Atom (a, "^" :: Term (b, rest)) ->
            Exp (a,b), rest
        | "-" :: Term (e, rest) -> // here so that -x^2 won't become (-x)^2
            Neg(e), rest
        | Atom (e, rest) ->
            e, rest

    and (|Atom|) tokens =
        match tokens with 
        | "(" :: (Expr id (e, rest)) ->
            match rest with
            | ")" :: rest -> e, rest
            | _ -> failwithf "Expected ')' at %A" rest        
        | Number(i) :: rest ->
            Const(i), rest
        | name :: rest -> 
            Var(name), rest 
        | _ -> failwithf "Unexpected token for atom: %A" tokens


    let parse str = 
        let tokens = str |> Tokenizing.tokenize |> List.ofSeq
        match tokens with
        | Expr id (e, leftover) -> 
            match leftover with
            | [] -> e
            | _ -> failwithf "Could not finish parsing input: %A" leftover