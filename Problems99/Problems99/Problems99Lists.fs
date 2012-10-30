//http://www.christiankissig.de/cms/index.php/en/programming/28-ocaml/28-99-problems-in-ocaml
//https://github.com/paks/99-FSharp-Problems
//#if COMPILED // when enabled the namespace is not recognized in the other tab
namespace Problems99
//#endif

module Utils = 
    let assertEqual x y msg = 
        if x <> y then
            failwith (sprintf "%s failed test. %A should be %A" msg x y)

    open System.Collections.Generic

    let memoize f = 
        let cache = Dictionary<_,_>(HashIdentity.Structural)
        fun args ->
            match cache.TryGetValue(args) with
            | (true, v) -> v
            | _ ->  let v = f(args)
                    cache.Add(args, v)
                    v

    // extend Map
    module Map = 
        // add multiple key-value pairs to the map
        let rec addItems items map = 
            List.fold (fun m (k,v) -> Map.add k v m) map items

    // extend Array2D
    module Array2D = 
        // add toSeq
        let toSeq arr = seq {
            for r in 0 .. (arr |> Array2D.length1) - 1 do
                for c in 0 .. (arr |> Array2D.length2) - 1 do
                    yield (r,c, arr.[r,c]) }


module Lists =     
    open Utils
    // 01 : Find the last element of a list.
    let rec last = function 
        | [x] -> x
        | h::t -> last t
        | _ -> invalidArg "list" "empty"

    assertEqual (last [1;2;3]) 3 "last"


    // 02 : Find the last but one element of a list. 
    let rec lastButOne = function 
        | x::[y] -> x
        | x::(y::t as rest) -> lastButOne rest
        | _ -> invalidArg "list" "not enough elements"

    assertEqual (lastButOne [1;2;3]) 2 "lastButOne"


    //03 : Find the K'th element of a list. The first element in the list is number 1. 
    let rec kth k = function
        | [] when k >= 1 -> invalidArg "list" "not enough elements"
        | h::t when k = 1 -> h
        | h::t when k > 1 -> kth (k-1) t
        | _ -> invalidArg "k" "wrong index"

    assertEqual (kth 2 [1;2;3]) 2 "kth"


    // 04 : Find the number of elements of a list. 
    let cnt lst = 
        List.fold (fun c _ -> c+1) 0 lst

    assertEqual (cnt [1;2;3]) 3 "cnt"


    // 05 : Reverse a list. 
    let rev lst = 
        let rec revTo lin lout = 
            match lin with            
            | [] -> lout
            | h::t -> revTo t (h::lout)            
        revTo lst []

    assertEqual (rev [1;2;3]) [3;2;1] "rev"


    // 06 : Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x). 
    let isPalindrome lst = 
        lst = rev lst

    assertEqual (isPalindrome [1;2;3]) false "isPalindrome"
    assertEqual (isPalindrome [1;2;3;2;1]) true "isPalindrome"


    // 07 : Flatten a nested list structure     
    // very hard to define or work with heterogenous lists
    (*
    let rec flatten<'T> (lst: obj) = 
        match lst with
        | :? List<'T> as l -> match l with 
                                | h::t -> (flatten h) @ (flatten t)
                                | [] -> []
        | _ -> [lst]

    let (flst: obj) = [1 :> obj; [2 :> obj; [3 :> obj; 4 :> obj] :> obj; 5 :> obj] :> obj] :> obj   
    let (fdlst: obj list) = [1;2;3;4;5]
    assertEqual (flatten flst) fdlst "flatten"
    *)

    let merge lst = 
        List.fold (fun accu lsti -> accu @ lsti) [] lst
    
    assertEqual (merge [[1;2;3];[4;5;6]]) [1; 2; 3; 4; 5; 6] "merge"

    // with discriminated union 
    type NL<'T> = 
    | N of 'T
    | L of NL<'T> list

    let rec flatten2 = function
    | N x -> [x]
    | L [] -> []
    | L lst -> List.collect flatten2 lst

    let nlst = L [N 1; L [N 2; L [N 3; N 4]; N 5]]
    assertEqual (flatten2 nlst) [1;2;3;4;5] "flatten2"

    let flatten3 lst = // tail recursive
        let rec loop acc = function
            | N x -> x::acc
            | L xs -> List.foldBack (fun li acc -> loop acc li) xs acc // from back to preserve order add each element and return updated accumulator
        loop [] lst

    assertEqual (flatten3 nlst) [1;2;3;4;5] "flatten3"


    // 08 : Eliminate consecutive duplicates of list elements. 
    let dedup lst = 
        List.fold (fun accu x -> match accu with
                                    | h::t when h = x -> accu
                                    | _ -> x::accu) [] lst
        |> rev

    assertEqual (dedup [1;1;1;2;3;3;4;2;2;1]) [1; 2; 3; 4; 2; 1] "dedup"


    // 09 : Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists. 
    let subdup lst = 
        let coll accu x = 
            match accu with            
            | (h::t as c)::ts when h = x -> (x::c)::ts
            | ts -> [x]::ts
        List.fold coll [] lst |> rev
    
    assertEqual (subdup [1;1;1;2;3;3;4;2;2;1]) [[1;1;1]; [2]; [3;3]; [4]; [2;2]; [1]] "subdup"


    // 10 : Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E. 
    let runLengthEncode lst = 
        lst 
        |> subdup 
        |> List.map (fun sl -> (List.length sl, List.head sl))

    assertEqual (runLengthEncode [1;1;1;2;3;3;4;2;2;1]) [(3, 1); (1, 2); (2, 3); (1, 4); (2, 2); (1, 1)] "runLengthEncode"


    // 11: Modified run-length encoding. Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists. 
    type RunLengthList<'T> = 
    | Single of 'T
    | Multi of int * 'T

    let toRunLengthOnDemand lst =
        lst |> List.map (function 
                        | (1,x) -> Single x 
                        | m -> Multi m )
    
    let runLengthEncodeOnDemand lst = 
        lst 
        |> runLengthEncode 
        |> toRunLengthOnDemand

    assertEqual (runLengthEncodeOnDemand [1;1;1;2;3;3;4;2;2;1]) [Multi (3,1); Single 2; Multi (2,3); Single 4; Multi (2,2); Single 1] "runLengthEncodeOnDemand"

    
    // 12: Decode a run-length encoded list. Given a run-length code list generated as specified in problem 11. Construct its uncompressed version. 
    let runLengthDecode lst = 
        let toList = function
            | Single e -> [e]
            | Multi(i,e) -> List.replicate i e
        List.collect toList lst

    assertEqual (runLengthDecode [Multi (3,1); Single 2; Multi (2,3); Single 4; Multi (2,2); Single 1]) [1;1;1;2;3;3;4;2;2;1] "runLengthDecode"


    // 13: Run-length encoding of a list (direct solution). Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
    let runLengthEncodeDirect lst = 
        let update accu item = 
            match accu with
            | (i,x)::t when x = item -> (i+1,x)::t
            | _ -> (1,item)::accu
        List.fold update [] lst 
        |> List.rev 
        |> toRunLengthOnDemand

    assertEqual (runLengthEncodeDirect [1;1;1;2;3;3;4;2;2;1]) [Multi (3,1); Single 2; Multi (2,3); Single 4; Multi (2,2); Single 1] "runLengthEncodeDirect"


    // 14: Duplicate the elements of a list. 
    let dupitems lst = 
        (lst,[]) ||> List.foldBack (fun i accu -> i::i::accu)

    assertEqual (dupitems [1;2;3]) [1;1;2;2;3;3] "dupitems"

    
    // 15: Replicate the elements of a list a given number of times. 
    let repitems n lst = 
        lst |> List.collect (fun i -> List.replicate n i)

    assertEqual (repitems 2 [1;2;3]) [1;1;2;2;3;3] "repitems"


    // 16: Drop every N'th element from a list. 
    let dropnth n lst = 
        let update accu x = 
            match accu with 
            | i, accu' when i = n -> 1, accu'
            | i, accu' -> i+1, x::accu'
        ((1,[]), lst) ||> List.fold update |> snd |> List.rev

    assertEqual (dropnth 2 [1;1;2;2;3;3]) [1;2;3] "dropnth"


    // 17: Split a list into two parts; the length of the first part is given. 
    let rec foldTo f n accu lst = 
        match n with 
        | 0 -> accu, lst
        | i when i > 0 -> match lst with
                            | h::t -> foldTo f (n-1) (f accu h) t
                            | [] -> invalidArg "n" "not enough elements"
        | _ -> invalidArg "n" "negative index"

    let splitat n lst = 
        let rh,t = foldTo (fun accu x -> x::accu) n [] lst
        rh |> List.rev, t

    assertEqual (splitat 2 [1;2;3;4]) ([1;2],[3;4]) "splitat"


    // 18: Extract a slice from a list. 
    let slice s e lst = 
        let d = s-1
        lst |> splitat d |> snd |> splitat (e-d) |> fst

    assertEqual (slice 3 5 [1;2;3;4;5;6]) [3;4;5] "slice"


    // 19: Rotate a list N places to the left. 
    let rotate n lst = 
        let l = List.length lst
        let c = (n % l) + (if n < 0 then l else 0)
        let h,t = splitat c lst
        t @ h

    assertEqual (rotate 2 [1;2;3;4;5]) [3;4;5;1;2] "slice"
    assertEqual (rotate -2 [1;2;3;4;5]) [4;5;1;2;3] "slice"


    // 20: Remove the K'th element from a list. 
    let removenth n lst = 
        match splitat (n-1) lst with
        | h, nth::t -> h @ t
        | _ -> invalidArg "n" "not enough elements"

    assertEqual (removenth 2 [1;2;3;4;5]) [1;3;4;5] "removenth"


    // 21: Insert an element at a given position into a list. 
    let insertat n i lst = 
        let h,t = splitat (n-1) lst
        h @ i::t

    assertEqual (insertat 3 3 [1;2;4;5]) [1;2;3;4;5] "insertat"


    // 22: Create a list containing all integers within a given range. 
    let range s e = [s .. e]


    // 23: Extract a given number of randomly selected elements from a list. 
    open System 

    let extract k lst = 
        match splitat (k-1) lst with
        | h, kth::t -> kth, h @ t
        | _ -> invalidArg "k" "not enough elements"

    assertEqual (extract 3 [1;2;3;4;5]) (3,[1;2;4;5]) "extract"

    let extractRnd n lst = 
        let rnd = new System.Random()
        let cnt = List.length lst
        let rec loop i accu rest = 
            if i = n then accu
            else   let k = rnd.Next(1, cnt-i+1)
                   let e, rest' = extract k rest
                   loop (i+1) (e::accu) rest'
        loop 0 [] lst

    assertEqual ((extractRnd 5 [1;2;3;4;5]) |> List.sort) [1;2;3;4;5] "extractRnd"


    let extractRndSort n lst = 
        let rnd = new System.Random()
        let rnds = seq{ while true do yield rnd.Next() }
        lst |> Seq.zip rnds |> Seq.sortBy fst |> Seq.take n |> Seq.map snd |> List.ofSeq


    // 24: Lotto: Draw N different random numbers from the set 1..M. 
    let draw n m = 
        (range 1 m) |> extractRnd n


    // 25: Generate a random permutation of the elements of a list.
    let permutate lst = 
        extractRnd (List.length lst) lst


    // 26: Generate the combinations of K distinct objects chosen from the N elements of a list. In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list. 
    let combinations' k lst = 
        // add a helper variable to hold the remaining list length
        let rec loop n k lst = 
            if k = 0 then 
                [[]] // only one way to choose zero elements. will be combined
            elif k = n then
                [lst] // only one way to choose all elements
            else [for i in 1 .. (n-k+1) do // starting with any head until only k elemens are left in the list
                    match splitat (i-1) lst with
                    | _, h::t -> for c in loop (n-i) (k-1) t do // combine head with the combinations of the tail
                                    yield h::c
                    | _ -> invalidArg "n" "not enough elements" ]
        let n = List.length lst
        loop n k lst

    let combinations k lst = 
        // add a helper variable to hold the remaining list length
        let rec loop n k lst = 
            if k = 0 then 
                [[]] // only one way to choose zero elements. will be combined
            elif k = n then
                [lst] // only one way to choose all elements
            else 
                match lst with
                | x::xs -> [for ys in loop (n-1) (k-1) xs do 
                                yield x::ys
                            if k < n then // combine the same length on the tail
                                yield! loop (n-1) k xs]
                | [] -> [[]]                
        let n = List.length lst
        loop n k lst

    assertEqual (combinations 2 [1;2;3]) [[1; 2]; [1; 3]; [2; 3]] "combinations"
    assertEqual (combinations' 2 [1;2;3]) [[1; 2]; [1; 3]; [2; 3]] "combinations'"
                    

    // 27: Group the elements of a set into disjoint subsets. 
    let rec group gs lst = 
        match gs with 
        | g::gs ->
            [for c in combinations g lst do
                let cs = Set.ofList c // filter those already combined
                let rest = lst |> List.filter (fun x -> not(Set.contains x cs))
                for rg in group gs rest do
                    yield c::rg]
        | [] -> [[]]

    let guys = "aldo beat carla david evi flip gary hugo ida".Split(' ') |> List.ofArray

    assertEqual (guys |> group [2;3;4] |> List.length) 1260 "group"
    assertEqual (group [1;2] [1;2;3]) [[[1]; [2; 3]]; [[2]; [1; 3]]; [[3]; [1; 2]]] "group"


    // 28: Sorting a list of lists according to length of sublists. 
    let subsort lst = 
        lst |> List.sortBy List.length

    assertEqual (subsort [[1;2;3]; [1]; [1;2]]) [[1];[1;2];[1;2;3]] "subsort"