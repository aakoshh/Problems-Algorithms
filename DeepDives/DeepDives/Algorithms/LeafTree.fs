namespace Algorithms.LeafTree

module Parsing = 
    
    /// tokenize (1,(23,456)) 
    let tokenize (s: string) =

        let (|AsString|) chr =
            chr.ToString()
        
        let (|Delimiter|_|) d =
            match d with
            | "(" | "," | ")" -> Some d
            | _ -> None

        let toString buffer = seq {
            if not <| List.isEmpty buffer then
                yield new System.String(buffer |> List.rev |> Array.ofList) 
        }

        let rec loop buffer (chars: char list) = seq {
            match chars with 
            | [] -> 
                yield! buffer |> toString
            | (AsString (Delimiter d)) :: rest->
                yield! buffer |> toString
                yield d
                yield! loop [] rest
            | c::rest ->
                yield! loop (c::buffer) rest
        }

        loop [] (s |> List.ofSeq)
            |> Seq.map (fun s -> s.Trim())


    let pop first tokens = 
        match tokens with
        | t::rest when t = first -> rest
        | _ -> failwith (sprintf "Expected %A at %A" first tokens)

                  
module BinaryLeafTree = 

    type 'a BinaryLeafTree = 
        | Leaf of 'a 
        | Branch of 'a BinaryLeafTree * 'a BinaryLeafTree
    

    let rec map f = function 
        | Leaf d -> Leaf (f d)
        | Branch(left,right) -> Branch(map f left, map f right)


    open Parsing
    /// Parse string representation (a,(b,c))
    let parse s =     
        let rec parseTree tokens cont = 
            match tokens with 
            | "("::rest ->
                parseTree rest <| fun left rest ->
                    parseTree (pop "," rest) <| fun right rest ->
                        cont (Branch(left,right)) (pop ")" rest)
            | data::(","::_ as rest)
            | data::(")"::_ as rest)
            | data::([] as rest) ->
                cont (Leaf data) rest
            | _ -> 
                failwith (sprintf "Could not parse %A" tokens)
        parseTree (s |> tokenize |> List.ofSeq) (fun t r -> t)



module MultiLeafTree =
    
    type 'a MultiLeafTree = 
        | Leaf of 'a 
        | Branch of 'a MultiLeafTree list


    let rec map f = function 
        | Leaf d -> Leaf (f d)
        | Branch trees -> Branch(trees |> List.map (map f))

    open Parsing
    /// ((a,b,c),((d,e),f))
    let parse s =                 
        let rec parseTree tokens cont = 
            match tokens with 
            | "("::rest ->
                parseBranches [] rest <| fun branches rest ->
                    cont (Branch branches) (pop ")" rest)
            | data::([] as rest) ->
                cont (Leaf data) rest
            | _ -> 
                failwith (sprintf "Could not parse %A" tokens)

        and parseBranches branches tokens cont = 
            match tokens with                       
            | "("::_ ->
                parseTree tokens <| fun tree rest ->
                    let branches = tree::branches
                    parseBranches branches rest cont
            | ","::rest ->
                parseBranches branches rest cont
            | ")"::_ ->
                cont (branches |> List.rev) tokens
            | data::(","::_ as rest) 
            | data::(")"::_ as rest) ->
                let branches = Leaf(data)::branches
                parseBranches branches rest cont 
            | _ -> 
                failwith (sprintf "Could not parse %A" tokens)

        parseTree (s |> Parsing.tokenize |> List.ofSeq) (fun t r -> t)



module Misc = 

    /// create all possible combinations of n pairs parentheses
    let rec parenthesize n = seq {
        if n = 0 then
            yield ""
        else
            for i in 0 .. n-1 do
                let j = n-1 - i
                for p1 in parenthesize i do
                    for p2 in parenthesize j do
                        yield sprintf "(%s)%s" p1 p2
    }  
    



module Tests = 

    open NUnit.Framework

    [<Test>]
    let TestTokenize() = 
        let t = Parsing.tokenize "(1,(23,456))" |> List.ofSeq
        let e = ["(";"1";",";"(";"23";",";"456";")";")"]
        Assert.AreEqual(e,t)


    module TestBinary = 
    
        open BinaryLeafTree

        [<Test>]
        let TestParse() =
            let t = parse "(1,(23,456))" |> BinaryLeafTree.map int
            let e = Branch (Leaf 1,Branch (Leaf 23,Leaf 456))
            Assert.AreEqual(e,t)

        [<Test>]
        let TestParseLeaf() =
            let t = parse "1"
            let e = Leaf "1"
            Assert.AreEqual(e,t)

        [<Test>]
        let TestParseComplex() = 
            let t = parse "((a,b),(c,(d,e)))"
            let e = Branch (Branch (Leaf "a",Leaf "b"),Branch (Leaf "c",Branch (Leaf "d",Leaf "e")))
            Assert.AreEqual(e,t)

    module TestMulti = 
        
        open MultiLeafTree

        [<Test>]
        let TestParseComplex() = 
            let t = parse "((a,b),(c,(d,e)),f)"
            let e =  Branch [Branch [Leaf "a"; Leaf "b"];
                             Branch [Leaf "c"; 
                                     Branch [Leaf "d"; Leaf "e"]]; 
                             Leaf "f"]
            Assert.AreEqual(e,t)


    module TestMisc = 
        
        open Misc

        [<Test>]
        let TestParentesize() = 
            let p = parenthesize 3 |> Set.ofSeq
            let e = ["()()()"; "()(())"; "(())()"; "(()())"; "((()))"] |> Set.ofList
            Assert.AreEqual(e,p)