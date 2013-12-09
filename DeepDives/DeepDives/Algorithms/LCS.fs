namespace Algorithms
open Algorithms.Common

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


    /// get the longest increasing subsequence from a list of numbers
    let longestInreasingSubsequence (lst: int list) = 
        let nums = lst |> Array.ofList
        // a function that returns the longest sequence ending at i
        let rec longest = memoize <| fun i ->
            let blank = [ 0,[] ] // if we have nothing to extend
            let extensible = [0 .. i-1] // any sequence from before we can append to
                            |> Seq.map (fun j -> longest j)
                            |> Seq.filter (fun (_, h::_) -> h < nums.[i])
            let l,s = Seq.append blank extensible |> Seq.maxBy fst
            l+1, nums.[i]::s
        longest (nums.Length-1) |> snd |> List.rev
            



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


        [<Test>]
        let TestLIS() = 
            Assert.AreEqual( [1;    2;   3;  4;    5],
                             [1;1;0;2;-1;3;2;4;3;4;5] |> longestInreasingSubsequence )
            Assert.AreEqual( 6, // not unique
                             [0; 8; 4; 12; 2; 10; 6; 14; 1; 9; 5; 13; 3; 11; 7; 15]
                                |> longestInreasingSubsequence |> List.length)
    


module SubArrays = 

    /// Sum of the sub-array with the largest sum with O(n^3) complexity.
    let largestSubSum (arr : int[]) = 
        let n = arr |> Array.length
        let sums = seq {
            for i in 0 .. n - 1 do
                for j in i .. n - 1 do
                    yield arr.[i .. j]
        }
        sums |> Seq.map Array.sum |> Seq.max


    /// Sum of the sub-array with the largest sum with O(n) complexity.
    let largestSubSumLin arr = 
        let n = arr |> Array.length
        let rec loop i rmax rsum = 
            if i = n then
                rmax
            else
                let rsum = max 0 (rsum + arr.[i]) 
                let rmax = max rsum rmax
                loop (i+1) rmax rsum
        loop 0 0 0



    module Tests = 

        open NUnit.Framework

        let cases = [[|1;2;3|], 6;
                     [|1;-1;2;-1|], 2;
                     [|3;-1;-4;10|], 10;]
                     |> List.map (fun (arr, lss) -> new TestCaseData(arr, lss))

        [<TestCaseSource("cases")>]
        let TestNaive(arr, lss) = 
            Assert.AreEqual(lss, largestSubSum arr)

        [<TestCaseSource("cases")>]
        let TestLinear(arr, lss) = 
            Assert.AreEqual(lss, largestSubSumLin arr)