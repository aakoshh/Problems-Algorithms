module Ch3

// find a small character surrounded by three big on either side

open System
open System.Net
open System.Text.RegularExpressions

let url = "http://www.pythonchallenge.com/pc/def/equality.html"
let client = new WebClient()
let source = client.DownloadString(url)

let characters = Regex.Matches(source, "<!--([^-]*)-->").[0].Groups.[1].Value


let (|Upper|Lower|Other|) char = 
    if Char.IsLower(char) then
        Lower char
    else if Char.IsUpper(char) then
        Upper char
    else 
        Other char

/// Find pattern in one row only
let findPattern chars = 
    let rec loop cs isFirst = 
        seq {
            match cs with
            | [] -> 
                yield! Seq.empty 
            |            (Upper a)::(Upper b)::(Upper c)::(Lower d)::(Upper e)::(Upper f)::(Upper g)::(Lower _)::_ when isFirst ->
                yield new String([|a;b;c;d;e;f;g|])
                yield! loop (cs |> List.tail) false
            | (Lower _)::(Upper a)::(Upper b)::(Upper c)::(Lower d)::(Upper e)::(Upper f)::(Upper g)::(Lower _)::_
            | (Lower _)::(Upper a)::(Upper b)::(Upper c)::(Lower d)::(Upper e)::(Upper f)::(Upper g)::[] ->
                yield new String([|a;b;c;d;e;f;g|])
                yield! loop (cs |> List.tail) false
            | c::rest -> 
                yield! loop rest (Char.IsWhiteSpace(c))            
        }
    loop chars true

let linearly = characters |> List.ofSeq |> findPattern |> List.ofSeq


/// All 4 directions
let matrix = 
    characters.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries) 
        |> Seq.map Array.ofSeq |> Array.ofSeq |> array2D

let findPattern4 matrix = 
    let n = matrix |> Array2D.length1
    let m = matrix |> Array2D.length2

    let guards (i,j) =        
        [i, j-3; i, j-2; i, j-1; 
            i, j+1; i, j+2; i, j+3; 
            i-3, j; i-2, j; i-1, j; 
            i+1, j; i+2, j; i+3, j;]

    let rec loop (i,j) = seq {
        if Char.IsLower(matrix.[i,j]) 
            && guards (i,j) 
                |> List.forall (fun (a,b) -> 
                    Char.IsUpper(matrix.[a,b])) then 
            yield i,j, matrix.[i,j]

        if j < m-1-3 then
            yield! loop (i, j+1)
        else if i < n-1-3 then
            yield! loop (i+1, 3)                
    }

    loop (3,3)


findPattern4 matrix |> List.ofSeq


// take out the middle of many occurrences
let middles = new String(linearly |> List.map (fun w -> w.[3]) |> Array.ofList)
printfn "%s" middles