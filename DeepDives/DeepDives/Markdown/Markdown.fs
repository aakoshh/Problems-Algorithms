namespace Markdown

/// Things that can appear within a block of text
type MarkdownSpan = 
    | Literal of string
    | HyperLink of MarkdownSpan list * string
    | InlineCode of string
    | Strong of MarkdownSpan list
    | Emphasis of MarkdownSpan list
/// Different types of paragraphs
and MarkdownBlock = 
    | Heading of int * MarkdownSpan list
    | Paragraph of MarkdownSpan list
    | CodeBlock of string list   
and MarkdownDocument =
    | MarkdownDocument of MarkdownBlock list


// we need functions to parse out delimited parts of text and lines starting with something.
// will treat lines as list of strings and a line as list of characters.
// recursive parser functions will try to match and return the rest of the text.

module Parsing = 

    let toChars str = str |> List.ofSeq
    let toString chars = new System.String(chars |> Array.ofList)

    /// Try to match the begining of a list of characters.
    let rec (|StartsWith|_|) prefix chars = 
        match prefix, chars with
        | [], rest -> // empty prefix matches anything
            Some(rest)
        | p::prest, c::crest when p = c ->
            (|StartsWith|_|) prest crest
        | _ -> 
            None

    /// Take characters until a condition is met.
    let takeUntil parser chars = 
        let rec loop acc chars = 
            match parser chars, chars with
            | Some(rest), _ -> 
                Some(acc |> List.rev, rest)
            | None, [] ->
                None
            | None, c::rest -> 
                loop (c::acc) rest
        loop [] chars



    /// See if the text can be enclosed by some character pairs
    let (|BracketedBy|_|) left right chars = 
        match chars with 
        | StartsWith left rest ->
            rest |> takeUntil ((|StartsWith|_|) right) 
        | _ -> None

    let (|Bracketed|_|) bracket = 
        (|BracketedBy|_|) bracket bracket


    let (|HyperLink|_|) chars = 
        match chars with 
        | BracketedBy ['['] [']'] (body,rest) ->
            match rest with // there can't be any spaces between [title](link)
            | BracketedBy ['('] [')'] (href,rest) -> 
                Some(body, href |> toString, rest)
            | _ -> None
        | _ -> None


    /// Try to parse a sequence of characters without newline into a list of spans.
    let rec parseSpans chars = 

        let toLiteral acc = seq {
            if acc <> [] then
                yield acc |> List.rev |> toString |> Literal
        }

        let emit acc item rest = seq {
            yield! acc |> toLiteral
            yield item
            yield! rest |> parseSpans
        }

        let rec loop acc chars = seq {
            match chars with
            | Bracketed ['*';'*'] (body,rest)
            | Bracketed ['_';'_'] (body,rest) ->
                let item = Strong( body |> parseSpans )
                yield! emit acc item rest

            | Bracketed ['*'] (body,rest) 
            | Bracketed ['_'] (body,rest)->
                let item = Emphasis( body |> parseSpans )
                yield! emit acc item rest 

            | Bracketed ['`'] (body,rest) ->
                let item = InlineCode( body |> toString )
                yield! emit acc item rest 

            | HyperLink (body,href,rest) -> 
                let item = HyperLink( body |> parseSpans, href )
                yield! emit acc item rest 

            | c::rest -> // could skip whitespace here
                yield! loop (c::acc) rest
            | [] -> 
                yield! acc |> toLiteral
        }

        loop [] chars |> List.ofSeq


    // a paragraph is a bunch of lines followd by a blank line.
    // until a blank line comes, everything belongs to the paragraph, 
    // even if it would be a code block or header.
    // double empty lines are skipped.


    let rec partitionWhile pred lst = 
        let rec loop acc lst = 
            match lst with 
            | head::rest when pred head ->
                loop (head::acc) rest
            | _ ->
                acc |> List.rev, lst
        loop [] lst

    /// To be able to match a head of a string list with nested character patterns.
    let (|AsChars|) str = 
        str |> toChars

    /// Get the lines that have a common prefix in the first list, rest in the second.
    let (|PrefixedLines|) prefix lines =
        let body, rest = lines |> partitionWhile (fun (x: string) -> x.StartsWith(prefix)) 
        let body = body |> List.map (fun (x: string) -> x.Substring(prefix |> String.length))
        body, rest
        

    /// Get the lines until the first emtpy line in the first list, rest in the second.
    let (|LineSeparated|) =
        partitionWhile (fun (x: string) -> not <| System.String.IsNullOrWhiteSpace(x))


    /// Recursive header pattern to try different depths.
    let (|Heading|_|) (marker,maxlvl) chars = 
        let rec loop d prefix = 
            match chars with 
            | StartsWith prefix head -> 
                Some(d, head)
            | _ when d < maxlvl ->
                loop (d+1) (marker::prefix)
            | _ -> None
        loop 1 [marker;' ']


    /// Given the rows of text, find blocks that belong together.
    let rec parseBlocks lines = seq {
        // see what matches first
        match lines with 
        | AsChars (Heading ('#',2) (lvl,head)) :: rest  ->
            yield Heading(lvl, head |> parseSpans)
            yield! rest |> parseBlocks

        | AsChars head :: AsChars (StartsWith ['=';'=';'='] _) :: rest ->
            yield Heading(1, head |> parseSpans)
            yield! rest |> parseBlocks
        | AsChars head :: AsChars (StartsWith ['-';'-';'-'] _) :: rest ->
            yield Heading(2, head |> parseSpans)
            yield! rest |> parseBlocks

        | PrefixedLines "    " (body, rest) when body <> [] ->
            yield CodeBlock(body)
            yield! rest |> parseBlocks

        | LineSeparated (body, rest) when body <> [] -> 
            let txt = body |> String.concat " "
            yield Paragraph(txt |> toChars |> parseSpans)
            yield! rest |> parseBlocks

        | x::rest when System.String.IsNullOrWhiteSpace(x) -> 
            yield! rest |> parseBlocks
        | [] -> ()            
    }

                
            
    


