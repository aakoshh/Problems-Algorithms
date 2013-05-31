namespace Symbolics.Tests

open NUnit.Framework
open Symbolics.Parsing.Tokenizing
open Symbolics.Parsing
open Symbolics


[<TestFixture>]
type ParsingTests() =     

    [<Test>]
    member x.TokenizingWorks() =         
        let tokens = tokenize "1 + x + (x'*3)^2 " |> List.ofSeq
        Assert.AreEqual(["1"; "+"; "x"; "+"; "("; "x'"; "*"; "3"; ")"; "^"; "2"], tokens)

    [<Test>]
    member x.SubtrationIsLeftAssociated() = 
        let exp = "x - y - z"
        let ast = Sub (Sub (Var "x",
                            Var "y"),
                       Var "z")
        Assert.AreEqual(ast, parse exp)

    [<Test>]
    member x.AssociativityCorrectlyRecognized() = 
        let exp = "3*x + y*z"
        let ast = Add (Mul (Const 3,Var "x"), Mul(Var "y",Var "z"))
        Assert.AreEqual(ast, parse exp)

    [<Test>]
    member x.CanMultiplyManyAtoms() = 
        let exp = "1 * 2 * 3"
        let ast = Mul (Const 1, Mul (Const 2, Const 3))
        Assert.AreEqual(ast, parse exp)

    [<Test>]
    member x.CanMultiplyWithGroup() = 
        let exp = "1 * (2 * 3)"
        let ast = Mul (Const 1, Mul (Const 2, Const 3))
        Assert.AreEqual(ast, parse exp)

    [<Test>]
    member x.CanDivideManyAtoms() = 
        let exp = "1 / 2 / 3"
        let ast = Div (Const 1, Div (Const 2, Const 3))
        Assert.AreEqual(ast, parse exp)

    [<Test>]
    member x.ExponentiationHasHigherPrecedenceThanMultiplication() = 
        let exp = "3 * x^(2+a) * 4 + 1"
        let ast = Add (Mul (Const 3, Mul(Exp (Var "x", Add (Const 2, Var "a")), Const 4)), Const 1)
        Assert.AreEqual(ast, parse exp)

    [<Test>]
    member x.CanNegateExpression() = 
        let exp = "-(x+y)"
        let ast = Neg (Add (Var "x", Var "y"))
        Assert.AreEqual(ast, parse exp)

    [<Test>]
    member x.CanNegateExponent() = 
        let exp = "-x^-2"
        let ast = Neg (Exp (Var "x", Neg (Const 2)))
        Assert.AreEqual(ast, parse exp)

    [<Test>]
    member x.CanChainExponents() = 
        let exp = "x^-y^2" // 
        let ast = Exp (Var "x", Neg (Exp (Var "y", Const 2)))
        Assert.AreEqual(ast, parse exp)

    [<Test>]
    member x.AllOperatorsAreCorrectlyParsed() = 
        let exp = "1 + x' + 1/x + 3*x^2 - x'^(-a/2) - 2"

        let s1 = Mul(Const 3, Exp(Var "x", Const 2))
        let s2 = Exp(Var "x'", Div(Neg(Var "a"), Const 2))

        let ast = Add(Const 1, 
                    Add(Var "x'", 
                        Add(Div(Const 1, Var "x"), 
                            Sub(Sub(s1, s2), Const 2))))
        Assert.AreEqual(ast, parse exp)
                            

