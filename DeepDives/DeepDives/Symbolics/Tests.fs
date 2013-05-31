namespace Symbolics.Tests

open NUnit.Framework
open Symbolics.Parsing.Tokenizing


[<TestFixture>]
type ParsingTests() =     

    [<Test>]
    member x.TokenizingWorks() =         
        let tokens = tokenize "1 + x + (x'*3)^2 " |> List.ofSeq
        Assert.AreEqual(["1"; "+"; "x"; "+"; "("; "x'"; "*"; "3"; ")"; "^"; "2"], tokens)

