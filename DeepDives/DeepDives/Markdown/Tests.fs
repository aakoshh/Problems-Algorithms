namespace Markdown.Tests

open NUnit.Framework
open Markdown
open Markdown.Parsing

[<TestFixture>]
type MarkdownTests() = 

    [<Test>]
    member x.TestStartsWith() = 
        let m = match toChars "... alma" with
                | StartsWith (toChars "...") rest -> rest
                | _ -> []                           
        Assert.AreEqual(toChars " alma", m)

        let m = match toChars "..." with
                | StartsWith (toChars "...") rest -> Some(rest)
                | _ -> None                        
        Assert.AreNotEqual(None, m)


    [<Test>]
    member x.TestBrackets() = 
        let body,rest = match toChars "<alma> bla" with
                | BracketedBy ['<'] ['>'] (body, rest) -> Some(body), Some(rest)
                | _ -> None, None
                        
        Assert.AreEqual(Some(toChars "alma"), body)
        Assert.AreEqual(Some(toChars " bla"), rest)


    [<Test>]
    member x.TestSpans() = 
        let parsed = "**important `code`** and _emphasized_" |> toChars |> parseSpans
        let expected = [Strong [Literal "important "; InlineCode "code"]; 
                        Literal " and ";
                        Emphasis [Literal "emphasized"]]           
        Assert.AreEqual(expected, parsed)
    

    [<Test>]
    member x.TestHyperLink() = 
        let parsed = "For more information, see the [**F#** home page](http://fsharp.net)." |> toChars |> parseSpans
        let expected = [Literal "For more information, see the ";
                        HyperLink ([Strong [Literal "F#"]; Literal " home page"],"http://fsharp.net"); Literal "."]
        Assert.AreEqual(expected, parsed)


    [<Test>]
    member x.TestPartition() =    
        Assert.IsTrue( partitionWhile (fun x -> x <> "f") ["a";"bc";"";"e"] = (["a";"bc";"";"e"],[]) )
        Assert.IsTrue( partitionWhile (fun x -> x <> "a") ["a";"bc";"";"e"] = ([],["a";"bc";"";"e"]) )
        Assert.IsTrue( partitionWhile (fun x -> x <> "") ["a";"bc";"";"e"] = (["a";"bc"], ["";"e"]) )


    [<Test>]
    member x.TestBlocks() = 
        let sample = """
                    # Introducing F#
                    F# is a _functional-first_ language,
                    which looks like this:

                        let msg = "world"
                        printfn "hello %s!" msg

                    This sample prints `hello world!`

                    Stay tuned
                    ===============
                    F# Deep Dives
                    """.Replace("                    ","")
                       .Replace("\r\n","\n")

        let input = sample.Split('\n') |> List.ofArray
        let parsed = input |> parseBlocks |> List.ofSeq
        let expected = [Heading (1,[Literal "Introducing F#"]);
                        Paragraph
                            [Literal "F# is a "; Emphasis [Literal "functional-first"];
                            Literal " language, which looks like this:"];
                        CodeBlock ["let msg = \"world\""; "printfn \"hello %s!\" msg"];
                        Paragraph [Literal "This sample prints "; InlineCode "hello world!"];
                        Heading (1,[Literal "Stay tuned"]);
                        Paragraph [Literal "F# Deep Dives"]]
        Assert.AreEqual( expected, parsed )

        
