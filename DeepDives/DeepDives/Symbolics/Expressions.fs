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


module Quotations = 

    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Quotations.DerivedPatterns
    
    let rec parse quotation = 
        
        let (|Operation|_|) op expr = 
            match expr with
            | SpecificCall op (_, _, exprList) ->
                let a = parse exprList.Head
                let b = parse exprList.Tail.Head
                Some(a,b)
            | _ -> None

        match quotation with
        | Int32(x) -> Const x
        | Operation <@@(+)@@> (a,b) -> Add (a,b)
        | Operation <@@(-)@@> (a,b) -> Sub (a,b)
        | Operation <@@(*)@@> (a,b) -> Mul (a,b)
        | Operation <@@(/)@@> (a,b) -> Div (a,b)
        | Operation <@@ pown @@> (a,b) -> Exp (a,b)
        | Call(None, op_UnaryNegation, [x]) -> Neg (parse x)
        | PropertyGet(_, propOrValInfo, _) -> Var(propOrValInfo.Name)
        | Var(x) -> Var(x.Name)
        | expr -> failwithf "Cannot parse quotation: %A" expr

    
module Printing = 

    let toString expr = 
        let paren above pr s = 
            if pr > above then sprintf "(%s)" s else s

        let rec loop pr e = 
            match e with
            | Add(a,b) -> sprintf "%s + %s" (loop 0 a) (loop 0 b) |> paren 0 pr
            | Sub(a,b) -> sprintf "%s - %s" (loop 0 a) (loop 1 b) |> paren 0 pr // Sub is left assoc so a - b - c - d will recurse into loop 0
            | Mul(a,b) -> sprintf "%s * %s" (loop 1 a) (loop 1 b) |> paren 1 pr
            | Div(a,b) -> sprintf "%s / %s" (loop 1 a) (loop 1 b) |> paren 1 pr
            | Exp(a,b) -> sprintf "%s^%s" (loop 2 a) (loop 2 b) // (a*b)^(c*d)
            | Neg e -> sprintf "-%s" (loop 1 e)
            | Const i -> sprintf "%d" i
            | Var x -> x 
        loop 0 expr       

    
    type Expression with
        member x.ToString() = 
            x |> toString


module Transformations = 

    let simplify expr =

        // simplify once
        let rec simplify' = function
            | Mul(Const 0, _)             
            | Mul(_, Const 0) -> Const 0
            | Add(Const 0, e)
            | Add(e, Const 0) 
            | Mul(Const 1, e)
            | Mul(e, Const 1)
            | Exp(e, Const 1) -> e
            | Exp(_, Const 0) -> Const 1

            | Add(Const a, Const b) -> Const (a+b)
            | Sub(Const a, Const b) -> Const (a-b)
            | Mul(Const a, Const b) -> Const (a*b)

            | Add(a, b) when a = b -> Mul(Const 2, a)
            | Sub(a, b) when a = b -> Const 0        
            | Mul(a, b) when a = b -> Exp(a, Const 2)
            | Div(a, b) when a = b -> Const 1

            | Neg(Neg e) -> e

            // recursive rules
            | Add(a,b) -> Add(simplify' a, simplify' b)
            | Sub(a,b) -> Sub(simplify' a, simplify' b)
            | Mul(a,b) -> Mul(simplify' a, simplify' b)
            | Div(a,b) -> Div(simplify' a, simplify' b)
            | Exp(a,b) -> Exp(simplify' a, simplify' b)
            | Neg(e) -> Neg(simplify' e)

            | e -> e

        // simplify as many times as it makes any changes
        let rec loop expr =  
            let expr' = simplify' expr
            if expr' = expr then expr else loop expr'

        loop expr


    let eval env expr = 
        let rec eval' = function
            | Add(a,b) -> eval' a + eval' b
            | Sub(a,b) -> eval' a - eval' b
            | Mul(a,b) -> eval' a * eval' b
            | Div(a,b) -> eval' a / eval' b
            | Exp(a,b) -> pown (eval' a) (eval' b)
            | Neg(e) -> -(eval' e)
            | Const(c) -> c
            | Var(x) -> env |> Map.tryFind x |> Option.get
        eval' expr