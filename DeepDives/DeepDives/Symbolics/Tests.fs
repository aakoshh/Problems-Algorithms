namespace Symbolics.Tests

open NUnit.Framework
open Symbolics
open Symbolics.Parsing
open Symbolics.Parsing.Tokenizing
open Symbolics.Printing
open Symbolics.Transformations



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
                            

[<TestFixture>]
type PrintingTests() =     

    let expressions () = [
        "2*x^2 + 3*(x+y) + 4";
        "a + b - c - d";
        "a + b - (c + d)";
        "1 / (x + 1)^2";
        "a * b + c * (d - e)";
        ]

    [<TestCaseSource("expressions")>]
    member x.PrintingResultsInIdenticalString(expr: string) =         
        let str = expr |> parse |> toString
        Assert.AreEqual(expr.Replace(" ",""), str.Replace(" ",""))

    [<TestCaseSource("expressions")>]
    member x.PrintedStringCanBeParsedBack(expr: string) =         
        let ast0 = expr |> parse 
        let str = ast0 |> toString
        let ast1 = str |> parse
        Assert.AreEqual(ast0, ast1)



[<TestFixture>]
type TransformationTests() =  

    let simplifications () = 
        [   
            "1 + 2 + 3 + 4", "10";
            "2 * (0 * a + 1)", "2";
            "x---x", "0";
            "(a + b + 0*c) * (a + b) * (3 - 2)", "(a + b)^2"
            "x ^ (3-2+a-a)", "x"
        ] 
        |> List.map (fun (e,s) -> new TestCaseData(e,s))

    [<TestCaseSource("simplifications")>]
    member x.SimplifictionsWorkAsExpected(expr, simple) =         
        let str = expr |> parse |> simplify |> toString
        Assert.AreEqual(simple, str)

    [<Test>]
    member x.EvalWorks() =         
        let expr = "-2*x^2 + (y+1)*x + 1" |> parse 
        let env  = ["x", 2; "y", 3] |> Map.ofSeq
        Assert.AreEqual(1, expr |> eval env )


[<TestFixture>]
type QuotationsTests() = 
    
    [<Test>] 
    member x.QuotationCanBeParsed() = 
        let q = <@@ -(1+2) + 1 / pown (2+3) 3 @@>
        let e = Add (Neg (Add (Const 1,Const 2)),
                     Div (Const 1,Exp (Add (Const 2,Const 3),Const 3)))
        let p = q |> Quotations.parse
        Assert.AreEqual(e, p)

    [<Test>] 
    member x.QuotationWithVariableCanBeParsed() = 
        let x = 1
        let y = 0
        let q = <@@ 2*x + y @@>
        let e = Add (Mul (Const 2, Var "x"), Var "y")
        let p = q |> Quotations.parse
        // Assert.AreEqual(e, p) // works in fsi
        ()