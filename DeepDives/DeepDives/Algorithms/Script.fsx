// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#nowarn "0040" // recursive memoization
#r "nunit.framework.dll"
#load "Common.fs"
#load "LeafTree.fs"
#load "LCS.fs"


// Define your library scripting code here

let rotate n arr = 
    let cnt = arr |> Array.length
    let i = n % cnt
    Array.append arr.[i..] arr.[..(i-1)]



[|0;1;2;3;4|] |> rotate 2 
[|0;1;2;3;4|] |> rotate 7 


open Algorithms.LeafTree

BinaryLeafTree.parse "((a,b),(c,(d,e)))"
MultiLeafTree.parse "((a,b),(c,(d,e)),f)"
MultiLeafTree.parse "((a,b),c)"


open Algorithms.LCS

longestInreasingSubsequence [1;1;0;2;-1;3;2;4;3;4;5]


longestInreasingSubsequence [0; 8; 4; 12; 2; 10; 6; 14; 1; 9; 5; 13; 3; 11; 7; 15]