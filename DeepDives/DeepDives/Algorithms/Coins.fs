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


    /// Change coins by getting rid of the smalles values greedily.
    /// Can result in inferior solutions like in case of [10; 20; 50] [1; 3; 1] 60
    let changeAndDisposeMostGreedy values pieces sum = 
        // create an array so we can use dynamic programming referring to indexes
        let coins = expandCoins values pieces 
                    |> Array.ofList 
                    |> Array.sort
        let n = coins |> Array.length

        let rec change = memoize <| fun (s, i) ->
            if i = n && s <> 0 then
                None
            else if s = 0 then
                Some (0, [])
            else if coins.[i] <= s then
                // try to go with the smallest coin
                match change (s-coins.[i], i+1) with
                | Some (w, cw) -> 
                    Some(w+1, coins.[i] :: cw) 
                | None -> 
                    change (s, i+1)
            else
                change (s, i+1)

        change (sum, 0)


    /// Start adding up the smalles coins and quit as soon as we have our sum.
    /// This is faster than doing the whole dynamic programming thing.
    let changeAndDisposeMostBuild values pieces sum = 
        // using smaller coins first
        let coins = expandCoins values pieces |> List.sort

        // build up a dictionary of sums with their parents and path length
        let sums = new System.Collections.Generic.Dictionary<int,int * int>()
        sums.[0] <- (0, 0) //  parent * path-length

        // go back and collect the increments we got
        let rec backtrack s steps =
            if s = 0 then
                steps
            else
                let prev = fst sums.[s] 
                let diff = s - prev
                backtrack prev (diff :: steps)
        
        // take coins one by one and build the sum dict
        let rec loop lst = 
            match lst with
            | [] -> // checked all the coins, now see what we got
                if sums.ContainsKey(sum) then Some(backtrack sum []) else None
            | hd :: tl -> 
                // no point keeping track of sums beyond our target
                let smallers = sums.Keys |> Seq.filter ((>) sum) |> List.ofSeq
                for i in smallers do
                    let child = i + hd
                    let steps = (snd sums.[i]) + 1
                    match sums.TryGetValue(child) with
                    | true, (_, steps') when steps' >= steps -> 
                        () // leave it if it's already longer
                    | _ -> 
                        sums.[child] <- (i, steps) // replace with longer or new
                loop tl
            
        match loop coins with
        | Some(coins) -> Some(coins |> List.length, coins)
        | None -> None


    
    
    module Submission = 
        open System
        open System.Collections.Generic
        open System.Net
        open System.Web
        open System.Collections.Specialized
        open System.Diagnostics

        /// Transform a dictionary into a query string.
        let encode values = 
            values 
                |> Map.fold 
                    (fun (query: NameValueCollection) k v -> query.[k] <- v; query) 
                    (HttpUtility.ParseQueryString("")) 
                |> (fun c -> c.ToString())
        
        /// Post a dictionary to a URL
        let post (url: string) values = 
            let client = new WebClient()
            let data = encode values
            client.Headers.["Content-Type"] <- "application/x-www-form-urlencoded"
            client.UploadString(url, data)


        /// Parse a JSON into a dictionary. 
        /// Alternatively we could create a record and annotate data member properties, 
        /// but the contents are not always known.
        let parse json = 
            let serializer = new System.Web.Script.Serialization.JavaScriptSerializer()
            let dict = serializer.Deserialize<IDictionary<string, obj>>(json) // lists will be obj arrays
            dict

        let toList<'T> (value: obj) = 
            value :?> obj[] |> List.ofArray |> List.map (fun x -> x :?> 'T)


        /// Fetch an exercise, solve it, and post back the results.
        let solve url (task: int) email = 
            let timer = Stopwatch.StartNew()
            let json = ["action", "fetch"; "number", string task] |> Map.ofSeq |> post url 
            printfn "fetched data at %d ms" timer.ElapsedMilliseconds
            printfn "%s" json

            let dict = parse json
            let uuid = dict.["uuid"] :?> string
            let price = dict.["price"] :?> int
            let values = dict.["values"] |> toList<int>
            let pieces = dict.["pieces"] |> toList<int>

            let solution = changeAndDisposeMostBuild values pieces price
            let answer = match solution with 
                | Some (cs, _) -> cs
                | _ -> 0

            printfn "found solution %d at %d ms" answer timer.ElapsedMilliseconds

            let result = ["action", "solve"; "number", string task; "uuid", uuid; "email", email; "solution", string answer] |> Map.ofSeq |> post url 
            
            printfn "received result at %d ms" timer.ElapsedMilliseconds
            printfn "%s" result



        /// Testing HTTP related conversions.
        module Tests = 
            open NUnit.Framework

            [<Test>]
            let JsonCanBeParsedIntoDictionary() = 
                let json = """{"price": 7607, "values": [1, 2, 5, 10, 20, 50, 100, 200], "uuid": "21688bd8-8722-4610-bfd4-8b10ffd1ab83", "pieces": [160, 138, 172, 146, 38, 25, 180, 107]}"""
                let dict = parse json
                Assert.AreEqual(7607, dict.["price"])
                Assert.AreEqual([1; 2; 5; 10; 20; 50; 100; 200], dict.["values"] |> toList<int>)
                Assert.AreEqual("21688bd8-8722-4610-bfd4-8b10ffd1ab83", dict.["uuid"])
                Assert.AreEqual([160; 138; 172; 146; 38; 25; 180; 107], dict.["pieces"] |> toList<int>)

            [<Test>]
            let ValuesCanBeSerializedIntoQueryString() = 
                let dict = ["uuid", "21688bd8-8722-4610-bfd4-8b10ffd1ab83";
                            "email", "me@gmail.com"] 
                            |> Map.ofSeq
                let data = encode dict
                Assert.AreEqual("email=me%40gmail.com&uuid=21688bd8-8722-4610-bfd4-8b10ffd1ab83", data)

                let back = HttpUtility.ParseQueryString(data)
                for (key,value) in dict |> Map.toSeq do
                    Assert.AreEqual(back.[key], value)


    /// Testing the algorithm.
    module Tests = 
        open NUnit.Framework
        open System.Diagnostics

        let correctMethods = [changeAndDisposeMost; changeAndDisposeMostBuild]

        let sampleValues = [1; 2; 5; 10; 20; 50; 100; 200]
        let samplePieces = [160; 138; 172; 146; 38; 25; 180; 107]
        let sampleValue  = 7606

        [<Test>]
        let CoinListsCanBeExpanded() = 
            let values = [200; 100; 50; 20; 5; 1]
            let pieces = [0; 3; 1; 6; 2; 4]
            Assert.AreEqual([100; 100; 100; 50; 20; 20; 20; 20; 20; 20; 5; 5; 1; 1; 1; 1], expandCoins values pieces)


        [<Test>]
        let ChangeSumsUpToValue() = 
            let coins  = changeAndDisposeMost sampleValues samplePieces sampleValue
            Assert.That(coins |> Option.isSome)
            Assert.AreEqual(sampleValue, coins |> Option.get |> snd |> List.sum)


        [<Test>]
        let CorrectAlgorithmsGiveEquivalentResults() = 
            let solutions = correctMethods |> List.map (fun f -> f sampleValues samplePieces sampleValue)
            let first = List.head solutions
            Assert.That(solutions |> List.tail |> List.forall (fun s -> 
                match first, s with
                | None, None -> true
                | Some (cc, cs), Some(fcc, fcs) -> cc = fcc && List.sum cs = List.sum fcs
                | _ -> false ))


        [<Test>]
        let DynamicAlgorithmFinishesWithin10Seconds() = 
            let timer  = Stopwatch.StartNew()
            let coins  = changeAndDisposeMost sampleValues samplePieces sampleValue
            let elapsed = timer.ElapsedMilliseconds
            Assume.That(elapsed < 10000L, sprintf "Should have finished faster than %d ms." elapsed) // not in debug mode
            printfn "Calculation took %d ms." elapsed 


        [<Test>]
        let BuilderAlgorithmFinishesWithin1Second() = 
            let timer  = Stopwatch.StartNew()
            let coins  = changeAndDisposeMostBuild sampleValues samplePieces sampleValue
            let elapsed = timer.ElapsedMilliseconds
            Assume.That(elapsed < 1000L, sprintf "Should have finished faster than %d ms." elapsed) // not in debug mode
            printfn "Calculation took %d ms." elapsed 


        [<Test>]
        let GreedyPicksInferiorSolution() = 
            let solution = changeAndDisposeMostGreedy [10; 20; 50] [1; 3; 1] 60
            Assert.AreEqual([10; 50], solution |> Option.get |> snd)

        [<Test>]
        let BuilderPicksTheRightSolution() = 
            let solution = changeAndDisposeMostBuild [10; 20; 50] [1; 3; 1] 60
            Assert.AreEqual([20;20;20], solution |> Option.get |> snd)


        [<Test>]
        let CorrectAlgorithmsPayExactAmountOrNothing() = 
            let values = [5; 10; 20]
            let pieces = [1; 2; 3]
            let value  = 37
            let solutions = correctMethods |> List.map (fun f -> f values pieces value)
            Assert.That(solutions |> List.forall Option.isNone)

        
        [<Test>]
        let KnownBaseProblemCanBeSolved() = 
            let values = [1; 2; 5; 10; 20; 50; 100; 200]
            let pieces = [25; 106; 85; 70; 2; 104; 60; 8]
            let value  = 5685
            let solution = changeAndDisposeMostBuild values pieces value
            Assert.AreEqual(371, solution |> Option.get |> fst)

        
        [<Test>]
        let EdgeCaseProblemCanBeSolved() = 
            let values = [4; 10; 19; 36; 45; 49; 50; 56; 64; 66; 85; 96; 103; 115; 122; 128; 137; 139; 143; 187]
            let pieces = [86; 25; 93; 96; 50; 79; 79; 30; 37; 92; 73; 75; 105; 30; 50; 105; 77; 14; 53; 72]
            let value  = 19700
            let solution = changeAndDisposeMostBuild values pieces value
            Assert.AreNotEqual(569, solution |> Option.get |> fst)
            Assert.AreEqual(571, solution |> Option.get |> fst)

        
        [<Test>]
        let SolutionExpandsToMoreLargerCoinsAddedToSmaller() = 
            let values = [4;36;56;64;66]
            let pieces = [2;1;2;1;2]
            let value  = 176
            let solution = changeAndDisposeMostBuild values pieces value 
            Assert.AreNotEqual(Some (3, [56; 56; 64]), solution) // stopping as soon as encountered
            Assert.AreEqual(Some (5, [4;4;36;66;66]), solution) 
            
