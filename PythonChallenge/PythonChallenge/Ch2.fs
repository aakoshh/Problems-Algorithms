module Ch2

// find rare characters in gibberish

open System
open System.Net
open System.Text.RegularExpressions

let url = "http://www.pythonchallenge.com/pc/def/ocr.html"
let client = new WebClient()
let source = client.DownloadString(url)

let mess = Regex.Matches(source, "<!--([^-]*)-->").[1].Groups.[1].Value

// analyze frequency
let rare = mess |> Seq.fold (fun freq item -> 
                    let cnt = 
                        match freq |> Map.tryFind item with
                        | Some(cnt) -> cnt
                        | None -> 0
                    freq |> Map.add item (cnt+1)) 
                    Map.empty
                |> Map.toSeq
                |> Seq.sortBy snd
                |> Seq.take 10
                |> List.ofSeq

let rares = rare |> List.filter (snd >> (=) 1) |> List.map fst |> Set.ofList

let hidden = new String(mess |> Seq.filter (fun c -> rares |> Set.contains c) |> Array.ofSeq)

printfn "%s" hidden
