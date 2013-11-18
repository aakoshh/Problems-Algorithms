module Ch2
open System

let data  = "g fmnc wms bgblr rpylqjyrc gr zw fylb. rfyrq ufyr amknsrcpq ypc dmp. bmgle gr gl zw fylb gq glcddgagclr ylb rfyr'q ufw rfgq rcvr gq qm jmle. sqgle qrpgle.kyicrpylq() gq pcamkkclbcb. lmu ynnjw ml rfc spj."

let decode s = 
    let trans = s |> Seq.map (function
        | ' ' -> ' '
        | c when not(Char.IsLetter(c)) -> c
        | c -> ((int c) - (int 'a') + 2) % 26 + (int 'a') |> (fun x -> char(x)))
    new String(trans |> Array.ofSeq)

printfn "%s" (decode data)
printfn "%s" (decode "map")