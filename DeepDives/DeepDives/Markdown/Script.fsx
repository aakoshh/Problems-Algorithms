// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Markdown.fs"
open Markdown
open Markdown.Parsing




let sample = 
        """
        Visual F#
        =========
        # What it is
        F# is a **programming language** that supports _functional_, as 
        well as _object-oriented_ and _imperative_ programming styles.
        Hello world can be written as follows:

            printfn "Hello world!"   

        Assignment looks like `let a = 1 + 2`.
                      
        ## See also...                        
        For more information, see the [F# home page] (http://fsharp.net) or 
        read [Real-World Functional Programming](http://manning.com/petricek) 
        published by [Manning](http://manning.com).
        """.Replace("        ","")       

parseBlocks (sample.Split([|'\n'|]) |> List.ofArray)

match toChars "... alma" with
    | StartsWith (toChars "...") rest -> rest
    | _ -> []

match toChars "<alma>" with
    | BracketedBy ['<'] ['>'] (body,rest) -> body
    | _ -> []

match toChars "<alma>" with
    | StartsWith (toChars "<") rest -> rest
    | _ -> []


['a'; 'l'; 'm'; 'a'; '>'] |> takeUntil (fun x -> if x |> List.head = '>' then Some( x |> List.tail) else None)
['a'; 'l'; 'm'; 'a'; '>'] |> takeUntil ((|StartsWith|_|) ['>']) 


"""
F# is a **programming language** that supports _functional_, as 
well as _object-oriented_ and _imperative_ programming styles.
Hello world can be written as follows:
""" |> toChars |> parseSpans


"""
For more information, see the [F# home page](http://fsharp.net).
""" |> toChars |> parseSpans

"**important `code`** and _emphasized_" |> toChars |> parseSpans
"For more information, see the [F# home page](http://fsharp.net)." |> toChars |> parseSpans


partitionWhile (fun x -> x <> "f") ["a";"bc";"";"e"]
