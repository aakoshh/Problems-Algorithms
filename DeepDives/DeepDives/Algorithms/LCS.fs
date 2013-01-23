namespace Algorithms
open Common

/// Longest Common Subsequence / Substring
module LCS = 

    /// Get the longest common subsequence of a and b (arrays of characters), not just the lenght.
    let longestCommonSubsequence a b =      
        let la, lb = Array.length a, Array.length b   
        let rec loop = memoize <| fun (i,j) ->
            // when we run out of either sequence, return
            if i < 0 || j < 0 then
                0, []
            // when the last characters match, extend the sequence
            elif a.[i] = b.[j] then
                let l, s = loop (i-1,j-1)
                l+1, a.[i]::s
            // when the last characters are different try shortening one of them
            else
                max (loop (i-1,j)) (loop (i,j-1))
        // work backwards
        let l, s = loop (la-1, lb-1)
        l, s |> List.rev


    /// Get the longest common consequent substring of two character arrays.
    let longestCommonSubstring a b = 
        // we don't know if we can extend a previous string, so basically we must
        // measure every common run and select the longest. this is perhaps easier to
        // visualize as a a*b table where every cell represents the length of the common
        // suffix ending at those substrings.
        let la, lb = Array.length a, Array.length b  
        let rec loop = memoize <| fun (i,j) ->
            if i < 0 || j < 0 then
                0, []
            // when the last character matches, we can take it and extend
            elif a.[i] = b.[j] then
                let l, s = loop (i-1,j-1)
                l+1, a.[i]::s
            // when the last caracter does not match, we must restart
            else
                0, []
        // enumerate every combination and select the longest (one of them)
        let l, s = seq {
                    for i in la-1 .. -1 .. 0 do
                        for j in lb-1 .. -1 .. 0 do
                            yield loop (i,j) 
                    } |> Seq.max
        l, s |> List.rev


    module Tests = 
        
        open NUnit.Framework

        [<Test>]
        let TestLCSubsequence() = 
            let a = "BANANA" |> Array.ofSeq
            let b = "ATANA" |> Array.ofSeq
            let l, s = longestCommonSubsequence a b
            Assert.AreEqual(4, l)
            Assert.IsTrue( ['A';'A';'N';'A'] = s )

        [<Test>]
        let TestLCSubstring() = 
            let a = "ABAB" |> Array.ofSeq
            let b = "BABA" |> Array.ofSeq
            let l, s = longestCommonSubstring a b
            Assert.AreEqual(3, l)
            Assert.IsTrue( ['B';'A';'B'] = s )
    
