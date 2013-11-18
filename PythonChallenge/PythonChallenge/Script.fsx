#r "System.Net"
#load "Ch1.fs"

open System.Xml.Linq

fsi.AddPrinter (fun (x: bigint) -> sprintf "%AI" x)