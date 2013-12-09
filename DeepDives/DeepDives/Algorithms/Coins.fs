namespace Algorithms
open Algorithms.Common

/// Exchange coins
module Coins = 

    /// Given two lists: the face values and the 
    /// number of coins of each value, produce a 
    /// common list of coins.
    let expandCoins values pieces = 
        (values, pieces) ||> List.zip
        |> List.map (fun (v, a) -> List.init a (fun _ -> v))
        |> List.collect id

    
    /// Uses the coins with the given face values, amounts given for each, 
    /// to produce a total of the given value, while getting rid of as many
    /// coins as possible.
    let changeAndDisposeMost values pieces sum = 
        // create an array so we can use dynamic programming referring to indexes
        let coins = expandCoins values pieces 
                    |> Array.ofList 
                    |> Array.sortBy (fun x -> -x)
        let n = coins |> Array.length

        /// Find the the bets way to give change
        /// for s using coins from the i-th index.
        let rec change = memoize <| fun (s, i) ->
            if i = n && s <> 0 then
                None
            else if s = 0 then
                Some (0, [])
            else if coins.[i] <= s then
                // see what happens if we use the coin or not
                let withCoin = change (s-coins.[i], i+1)
                let withoutCoin = change (s, i+1)

                match withCoin, withoutCoin with
                | Some (w, cw), Some(wo, _) when w >= wo -> 
                    Some(w+1, coins.[i] :: cw) 
                | _, Some _ -> 
                    withoutCoin
                | Some (w, cw), _ -> 
                    Some(w+1, coins.[i] :: cw)
                | None, None ->
                    None
            else
                change (s, i+1)

        change (sum, 0)


    module Tests = 
        open NUnit.Framework
        open System.Diagnostics

        [<Test>]
        let CoinListsCanBeExpanded() = 
            let values = [200; 100; 50; 20; 5; 1]
            let pieces = [0; 3; 1; 6; 2; 4]
            Assert.AreEqual([100; 100; 100; 50; 20; 20; 20; 20; 20; 20; 5; 5; 1; 1; 1; 1], expandCoins values pieces)


        [<Test>]
        let ChangeSumsUpToValue() = 
            let values = [1; 2; 5; 10; 20; 50; 100; 200]
            let pieces = [160; 138; 172; 146; 38; 25; 180; 107]
            let value  = 7606
            let coins  = changeAndDisposeMost values pieces value
            Assert.That(coins |> Option.isSome)
            Assert.AreEqual(value, coins |> Option.get |> snd |> List.sum)


        [<Test>]
        let AlgorithmFinishesWithing10Seconds() = 
            let values = [1; 2; 5; 10; 20; 50; 100; 200]
            let pieces = [160; 138; 172; 146; 38; 25; 180; 107]
            let value  = 7606
            let timer  = Stopwatch.StartNew()
            let coins  = changeAndDisposeMost values pieces value
            let elapsed = timer.ElapsedMilliseconds
            Assume.That(elapsed < 10000L, sprintf "Should have finished faster than %d ms." elapsed) // not in debug mode
            printfn "Calculation took %d ms." elapsed 