namespace Algorithms

module Sorting = 
    
    let swap (arr: 'a[]) i j = 
        let t = arr.[i]
        arr.[i] <- arr.[j]
        arr.[j] <- t  

    // move every element that is smaller than arr.[p] to the front, largers back
    let partition (arr: 'a[]) l r p = 
        let mutable i = l
        let mutable j = r
        let mutable p = p
        let pivot = arr.[p]
        // until i and j meet, swap elements in pairs. when they meet, that's where the pivot is
        while i < j do
            // seek the first larger element on the left (will skip the pivot, so that won't move)
            while arr.[i] <= pivot && i <  j do i <- i + 1 
            // seek the first smaller element on the right (or stop at the last smaller if i = j) 
            while arr.[j] >= pivot && i <= j do j <- j - 1 
            if i < j then 
                swap arr i j
            else // at this point i points to the first larger, j to the last smaller
                if p > i then
                    swap arr p i
                    p <- i
                elif p < j then
                    swap arr p j
                    p <- j
        p // return where the pivot ended up being


    /// in-place quicksort of an array
    let quicksort (arr: 'a[]) = 
        // here we could randomize the array to avoid the worst case n**2
        let rec qsort i j =             
            if i < j then
                // pivot around the middle point
                let p = (i+j)/2
                // move the smaller elements before, larger after the pivot
                // return the final position of where the pivot went
                let p = partition arr i j p
                // sort the two parts individually, could be parallel
                qsort i (p-1)
                qsort (p+1) j
        qsort 0 ((Array.length arr)-1)  
        arr      



    module Tests = 
        open NUnit.Framework

        [<Test>]
        let TestQSort() =             
            let arr = [|7;3;1;4;6;8;5|]            
            Assert.AreEqual( [|1; 3; 4; 5; 6; 7; 8|], quicksort arr )

