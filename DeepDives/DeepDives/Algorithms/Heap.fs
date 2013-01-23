namespace Algorithms

module Heap = 

    type 'a Heap = 
        | Node of 'a * int * 'a Heap * 'a Heap
        | Empty


    let rec weight = function
        | Node(_, w, _, _) -> w
        | Empty -> 0


    let toleft = function 
        | Node(d, w, left, right) when weight left < weight right ->
            Node(d, w, right, left)
        | h -> h


    let top = function 
        | Empty -> None
        | Node(x, _, _, _) -> Some(x)


    /// insert x into the heap while keeping the tree balanced.
    let rec insert x = function
        | Empty -> 
            Node(x, 1, Empty, Empty)
        | Node(y, w, left, right) when x >= y ->
            Node(y, w+1, left, insert x right) |> toleft
        | Node(y, w, left, right) -> // x < y
            Node(x, w+1, left, insert y right) |> toleft


    let rec merge a b = 
        match a, b with
        | _, Empty -> a
        | Empty, _ -> b
        | Node(da,wa,la,ra), Node(db,wb,lb,rb) ->
            if da <= db then
                Node(da, wa+wb, merge la ra, b) |> toleft
            else
                Node(db, wa+wb, a, merge lb rb) |> toleft


    /// remove the top element and return the smaller heap
    let pop = function
        | Empty -> 
            Empty
        | Node(_, _, rest, Empty) 
        | Node(_, _, Empty, rest) -> 
            rest
        | Node(_, _, left, right)  ->
            merge left right |> toleft


    /// enumerate in increasing order
    let rec toSeq heap = seq {
        match top heap with 
        | Some(x) -> 
            yield x
            yield! pop heap |> toSeq  
        | None -> ()          
    }


    /// create from list
    let ofSeq s = 
        s |> Seq.fold (fun heap i -> insert i heap) Empty
            


    module Tests = 
        
        open NUnit.Framework

        [<Test>]
        let TestTop() = 
            let heap = [1;3;5;4;7;9] |> ofSeq
            Assert.AreEqual(Some(1), top heap)
            Assert.AreEqual(Some(5), heap |> pop |> pop |> pop |> insert 8 |> top)




