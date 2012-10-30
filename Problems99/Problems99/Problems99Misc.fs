namespace Problems99

module Misc = 
    open Problems99.Utils

    // Problem 90 : Eight queens problem
    // [4,2,7,3,6,8,5,1] -> queen in 1. column is in the 4. row
    let queens n =         
        // first fill the first column, put the first queen in all available positions
        // then try to add the second queen in all positions by checking if it violates the ones before that
        // check if two position shit each other
        let hits (r1,c1) (r2,c2) = 
            r1 = r2 || c1 = c2 || abs(c2 - c1) = abs(r2 - r1)
        // check if considering the list of already placed queens a col and row can be placed
        let valid placed col row = 
            // placed will be in reverse order and is assumed that col is the next, no gaps
            placed |> Seq.mapi (fun i r -> hits (r,col-1-i) (row,col)) // are there any of the existing that is hit
                   |> Seq.exists id |> not // there should not be any
        // try to add fill the next column in all possible valid ways
        let rec loop placed col = 
            seq{ // return the first finds as soon as possible 
                if col > n then // found a solution
                    yield placed |> List.rev
                else // try to add the queen in col
                    for row in 1 .. n do
                        if valid placed col row then
                            yield! loop (row::placed) (col+1) }
        loop [] 1

    assertEqual (queens 8 |> Seq.length) 92 "queens"
    assertEqual (queens 8 |> Seq.head) [1; 5; 8; 6; 3; 7; 2; 4] "queens"
    //assertEqual (queens 20 |> Seq.head) [1; 3; 5; 2; 4; 13; 15; 12; 18; 20; 17; 9; 16; 19; 8; 10; 7; 14; 6; 11] "queens"


    // Problem 91 : Knight's tour
    // find tours ending at a particular square
    let knightsTour n ((r,c) as endsquare) = 
        // enumerate all the possible moves from a cell
        let moves (r,c) = // very slow compared to moves'
            seq{for dr in [1;-1] do
                    for dc in [1;-1] do 
                        yield (r+dr*1, c+dc*2)
                        yield (r+dr*2, c+dc*1) } 
                |> Seq.filter (fun (r,c) -> 1 <= r && r <= n && 1 <= c && c <= n)
        let moves' (r,c) =
            [(r+2,c+1);(r+2,c-1);(r-2,c+1);(r-2,c-1);(r-1,c+2);(r-1,c-2);(r+1,c+2);(r+1,c-2)] 
            |> List.filter(fun (r,c) -> r > 0 && r <= n && c > 0 && c <= n)
        // starting or ending does not matter so we can start from (r,c) and not reverse the path at the end
        let rec loop cnt path visited = seq {
            if cnt = n*n then // visited every cell on the board
                yield path
            else // for every move from the last cell not yet visited try to go
                let curr = path |> List.head
                let validMoves = curr |> moves' |> Seq.filter (fun cell -> visited |> Set.contains cell |> not)
                // according to Warnsdorff's rule we have to first try to route wich leaves us with the least possible options
                for cell in validMoves |> Seq.sortBy (moves' >> Seq.length) do // without this it would not finish for 8x8
                    yield! loop (cnt+1) (cell::path) (visited |> Set.add cell) }
        loop 1 [endsquare] (endsquare |> Set.singleton)

    
    assertEqual (knightsTour 8 (1,1) |> Seq.head) [(4, 3); (6, 4); (5, 6); (4, 8); (3, 6); (5, 5); (6, 3); (4, 4); (2, 3);
                                                   (1, 5); (3, 4); (5, 3); (6, 5); (4, 6); (2, 7); (3, 5); (5, 4); (6, 6);
                                                   (4, 5); (2, 4); (1, 6); (2, 8); (4, 7); (6, 8); (8, 7); (7, 5); (8, 3);
                                                   (7, 1); (5, 2); (3, 1); (1, 2); (3, 3); (4, 1); (2, 2); (1, 4); (2, 6);
                                                   (1, 8); (3, 7); (5, 8); (7, 7); (8, 5); (7, 3); (8, 1); (6, 2); (7, 4);
                                                   (8, 2); (6, 1); (4, 2); (2, 1); (1, 3); (2, 5); (1, 7); (3, 8); (5, 7);
                                                   (7, 8); (8, 6); (6, 7); (8, 8); (7, 6); (8, 4); (7, 2); (5, 1); (3, 2);
                                                   (1, 1)] "knightsTour"

    // tour that starts where it ends
    let closedKnightsTour n = 
        // if we find a circle all points on it are valid and since they contain the first point it does not matter where we start from
        let start = (1,1) // the valid end positions are [3,2; 2;3]        
        knightsTour n start 
        |> Seq.find (fun path -> 
            let finish = path |> List.head
            finish = (3,2) || finish = (2,3)) 


    assertEqual (closedKnightsTour 8) [(2, 3); (4, 4); (6, 3); (5, 5); (4, 3); (6, 4); (5, 6); (4, 8); (3, 6);
                                       (1, 5); (3, 4); (5, 3); (6, 5); (4, 6); (2, 7); (3, 5); (5, 4); (6, 6);
                                       (4, 5); (2, 4); (1, 6); (2, 8); (4, 7); (6, 8); (8, 7); (7, 5); (8, 3);
                                       (7, 1); (5, 2); (3, 1); (1, 2); (3, 3); (4, 1); (2, 2); (1, 4); (2, 6);
                                       (1, 8); (3, 7); (5, 8); (7, 7); (8, 5); (7, 3); (8, 1); (6, 2); (7, 4);
                                       (8, 2); (6, 1); (4, 2); (2, 1); (1, 3); (2, 5); (1, 7); (3, 8); (5, 7);
                                       (7, 8); (8, 6); (6, 7); (8, 8); (7, 6); (8, 4); (7, 2); (5, 1); (3, 2);
                                       (1, 1)] "closedKnightsTour"


    // Problem 92 : Von Koch's conjecture
    //                                         6
    //        (d)   (e)---(f)        (4)   (1)---(7)
    //         |     |              1 |     | 5
    //        (a)---(b)---(c)        (3)---(6)---(2)
    //         |                    2 |  3     4
    //        (g)                    (5)

    type 'a Graph = 'a list * ('a * 'a) list
    // sort edges in a way that there are no islands
    let treeSort edges = 
        let total = edges |> Seq.length
        let rec loop cnt path ev nv = 
            if cnt = total then
                path |> List.rev
            else
                // find an edge that comes from the current node or if there is none then it comes from an existing
                let ca,cb = path |> List.head
                let (a,b) as next = edges 
                                 |> Seq.filter (fun e -> ev |> Set.contains e |> not) // edge not visited
                                 |> Seq.filter (fun (a,b) -> nv |> Set.contains a || nv |> Set.contains b ) // one node is visited   
                                 |> Seq.sortBy (function
                                    | a,b when a = cb -> 0 // goes from current
                                    | a,b when a = ca -> 1 // only one back from current
                                    | a,b when b = ca || b = cb -> 2 // nodes listed backwards
                                    | _ -> 3)  
                                 |> Seq.head 
                loop (cnt+1) (next::path) (ev |> Set.add next) (nv |> Set.add a |> Set.add b)
        let a,b as start = edges |> Seq.min       
        loop 1 [start] (Set.singleton start) ([a;b] |> Set.ofList)


    let vanKoch ((n,e): Graph<_>) = 
        // sort edges
        let edges = treeSort e
        // remaining node and edge numbers
        let ni = [1 .. n |> List.length] |> Set.ofList // n
        let ei = [1 .. e |> List.length] |> Set.ofList // n-1
        let getNodeNum node assigned free = 
            match Map.tryFind node assigned with
            | Some(n) -> Set.singleton n
            | None -> free
        // for each edge try to assign a number
        let rec loop edges nass eass nfree efree = seq {
            match edges with
            | [] -> yield (nass,eass)                               
            | ((a,b) as e)::rest ->          
                // try every edge number not yet assigned
                for ex in efree do
                    // get the node numbers. if they are assigned, use that, otherwise free
                    for ax in getNodeNum a nass nfree do
                        for bx in getNodeNum b nass (nfree |> Set.remove ax) do
                            if ex = abs(bx - ax) then
                                // a possible choice, try to go deeper
                                yield! loop rest // rest of the edges
                                            (nass |> Map.add a ax |> Map.add b bx) // assign the node numbers
                                            (eass |> Map.add e ex) // assign edge number
                                            (nfree |> Set.remove ax |> Set.remove bx) // remove from free node numbers
                                            (efree |> Set.remove ex) } // remove from free edge numbers 
        loop edges Map.empty Map.empty ni ei |> Seq.map (fun (ni,ei) ->
                                                    n |> List.map (fun n -> ni.[n]), 
                                                    e |> List.map (fun e -> ei.[e]))

    let g7 = (['d';'a';'g';'b';'c';'e';'f'],
              [('d', 'a');('a', 'g');('a', 'b');('b', 'e');('b', 'c');('e', 'f')])
    let g14 = (['i';'h';'g';'a';'b';'d';'c';'f';'k';'e';'q';'m';'p';'n'],
               [('i', 'a');('h', 'a');('a', 'b');('a', 'g');('a', 'c');('c', 'f');('c','d');('d','k');('c','e');('e','q');('q','m');('q','n');('n','p')])
    //treeSort [('d', 'a');('e', 'f');('b', 'c');('a', 'g');('b', 'e');('a', 'b')]    
    //g7 |> vanKoch |> Seq.head
    //g14 |> vanKoch |> Seq.head


    // Problem 93 : An arithmetic puzzle    

    // all possible splits of a list
    let splits lst = 
        [for i in 1 .. (lst |> List.length)-1 do 
            yield lst |> Lists.splitat i]

    assertEqual (splits [1;2;3;4]) [([1], [2; 3; 4]); ([1; 2], [3; 4]); ([1; 2; 3], [4])] "splits"

    type Op = | Eq | Add | Sub | Mul | Div 

    type Expr = 
        | Term of Op * Expr * Expr 
        | Var of BigRational // for division in F# Power Pack
            
    // evaluate an expression
    let rec eval expr = 
        match expr with
        | Var(x) -> x
        | Term(op, lexp, rexp) ->
            let lh = eval lexp
            let rh = eval rexp
            match op with 
            | Add -> lh + rh
            | Sub -> lh - rh
            | Mul -> lh * rh
            | Div -> lh / rh
            | Eq  -> failwith "= is not an arithmetic expression"

    // check that an equation holds
    let equate expr = 
        match expr with
        | Term(Eq, lexp, rexp) ->
            try 
                eval lexp = eval rexp 
            with
            | :? System.DivideByZeroException as ex -> 
                false
        | _ -> invalidArg "expr" "not an equation"

    let exprToString expr = 
        let opToString op = 
            match op with 
            | Eq -> "=" | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" 
        let rec loop lvl expr =
            match expr with
            | Var(x) -> x.ToString()
            | Term(Eq, lexp, rexp) -> 
                sprintf "%s = %s" (lexp |> loop (lvl+1)) (rexp |> loop (lvl+1))
            | Term(op,lexp,rexp) -> 
                let fmt = if lvl > 1 then sprintf "(%s %s %s)" else sprintf "%s %s %s"
                fmt (lexp |> loop (lvl+1)) (op |> opToString) (rexp |> loop (lvl+1))
        loop 0 expr

    // build all possible expressions
    let expressions nums = 
        let rec build ns = seq {
            match ns with
            | [x] -> yield Var(BigRational.FromInt(x))
            | [] -> failwith "cannot build from empty list"
            | _ -> // subdivide
                for (h,t) in ns |> splits do
                    for lexp in h |> build do
                        for rexp in t |> build do
                            for op in [Add;Sub;Mul;Div] do // avoid divide by zero here or in check? multiply by one? redundant parantheses?
                                yield Term(op, lexp, rexp) }
        seq { // all possible equations (where to put the '=')
        for (h,t) in nums |> splits do
            for lexp in h |> build do
                for rexp in t |> build do
                    yield Term(Eq, lexp, rexp) }
        
    
    let arithmeticPuzzle nums = 
        nums |> expressions |> Seq.filter equate 

    let puzzleSolutions = [2;3;5;7;11] |> arithmeticPuzzle
    // puzzleSolutions |> Seq.iter (fun e -> printfn "%s" (e |> exprToString))    
    assertEqual (puzzleSolutions |> Seq.length) 12 "arithmeticPuzzle"


    // Problem 97 : Sudoku
    let sudokuIn = array2D [[0;0;4;8;0;0;0;1;7];
                            [6;7;0;9;0;0;0;0;0];
                            [5;0;8;0;3;0;0;0;4];
                            [3;0;0;7;4;0;1;0;0];
                            [0;6;9;0;0;0;7;8;0];
                            [0;0;1;0;6;9;0;0;5];
                            [1;0;0;0;8;0;3;0;6];
                            [0;0;0;0;0;6;0;9;1];
                            [2;4;0;0;0;1;5;0;0]]

    let sudokuOut = array2D[[9; 3; 4; 8; 2; 5; 6; 1; 7]
                            [6; 7; 2; 9; 1; 4; 8; 5; 3]
                            [5; 1; 8; 6; 3; 7; 9; 2; 4]
                            [3; 2; 5; 7; 4; 8; 1; 6; 9]
                            [4; 6; 9; 1; 5; 3; 7; 8; 2]
                            [7; 8; 1; 2; 6; 9; 4; 3; 5]
                            [1; 9; 7; 5; 8; 2; 3; 4; 6]
                            [8; 5; 3; 4; 7; 6; 2; 9; 1]
                            [2; 4; 6; 3; 9; 1; 5; 7; 8]]

    //        .  .  4 | 8  .  . | .  1  7          9  3  4 | 8  2  5 | 6  1  7         
    //                |         |                          |         |
    //        6  7  . | 9  .  . | .  .  .          6  7  2 | 9  1  4 | 8  5  3
    //                |         |                          |         |
    //        5  .  8 | .  3  . | .  .  4          5  1  8 | 6  3  7 | 9  2  4
    //        --------+---------+--------          --------+---------+--------
    //        3  .  . | 7  4  . | 1  .  .          3  2  5 | 7  4  8 | 1  6  9
    //                |         |                          |         |
    //        .  6  9 | .  .  . | 7  8  .          4  6  9 | 1  5  3 | 7  8  2
    //                |         |                          |         |
    //        .  .  1 | .  6  9 | .  .  5          7  8  1 | 2  6  9 | 4  3  5
    //        --------+---------+--------          --------+---------+--------
    //        1  .  . | .  8  . | 3  .  6          1  9  7 | 5  8  2 | 3  4  6
    //                |         |                          |         |
    //        .  .  . | .  .  6 | .  9  1          8  5  3 | 4  7  6 | 2  9  1
    //                |         |                          |         |
    //        2  4  . | .  .  1 | 5  .  .          2  4  6 | 3  9  1 | 5  7  8

    module Sudoku =                             
        type SudokuTable = int [,] // 2D array
        exception SolutionException of SudokuTable
        // check that the given digit can be placed in (row,col) in table (zero based)
        // there can be only one digit from everything in every row/col/square        
        let valid (table : SudokuTable) (row,col) digit =         
            table.[row,col] = 0 && // not taken yet
            seq {         
            for c in 0 .. 8 do // in the same row
                yield table.[row, c] 
            for r in 0 .. 8 do // in the same col
                yield table.[r, col]
            let sr, sc = row / 3, col / 3
            for r in sr * 3 .. sr * 3 + 2 do // in the same square
                for c in sc * 3 .. sc * 3 + 2 do
                    yield table.[r, c] }
            |> Seq.exists ((=) digit) |> not // the given digit is not yet in the row, col or square

        // count the number of elements per row and column        
        let count table = 
            let rc = table |> Array2D.length1 |> Array.zeroCreate
            let cc = table |> Array2D.length2 |> Array.zeroCreate
            table |> Array2D.iteri (fun r c d ->
                if d <> 0 then
                    rc.[r] <- rc.[r] + 1
                    cc.[c] <- cc.[c] + 1)
            let toMap arr = 
                arr |> Array.toSeq |> Seq.mapi (fun i d -> (i,d)) |> Map.ofSeq
            rc, cc // keep them as arrays like the main table

        // collect free cells
        let freeCells table = 
            table |> Array2D.toSeq // in Utils
                  |> Seq.filter (fun (r,c,d) -> d = 0)
                  |> Seq.map (fun (r,c,d) -> (r,c))
                  |> Set.ofSeq
        
        // find the solution to a 2D array
        let solve table =             
            // if we use an array for convenient index based access, we lose immutability and easy backtracking
            // but it can set back manually after the recursive call ends. when a solution if found we can use an exception, or a choice
            // I will always try to recurse in the cell where the rows and cols contain the most elements (least choice for the cell)
            let rec loop table (rc: int[]) (cc: int[]) free = 
                if free |> Set.isEmpty then
                    raise( SolutionException(table) ) // or return Choice1of2
                else // loop free cells ordered by most count first
                    //let empty = free |> Set.toSeq |> Seq.sortBy (fun (r,c) -> -1 * (rc.[r] + cc.[c])) // too slow
                    // a priority queue would be better but it does not provide much advantage than the simple row-by-row below, and maintaining is slower
                    let empty = [free.MinimumElement] 
                    seq { // try to fill the empty cells with any number that is valid there 
                    for (r,c) in empty do
                        for d in 1 .. 9 do
                            if valid table (r,c) d then
                                // fill the table cell
                                table.[r,c] <- d    
                                rc.[r] <- rc.[r] + 1                            
                                cc.[c] <- cc.[c] + 1
                                yield! loop table rc cc (free |> Set.remove (r,c))
                                // if we are here there was no solution, so backtrack
                                table.[r,c] <- 0    
                                rc.[r] <- rc.[r] - 1                            
                                cc.[c] <- cc.[c] - 1 }
            // preserve the table
            let table = table |> Array2D.copy
            // count the rows
            let rc,cc = table |> count
            // gather the free cells
            let free = table |> freeCells
            // start
            try 
                loop table rc cc free |> Seq.head
            with
            | SolutionException(solution) -> Some(solution)
            | :? System.ArgumentException -> None // empty sequence no head
                 

    module Sudoku2 =                             
        type SudokuTable = int [,] // 2D array
        // check that the given digit can be placed in (row,col) in table (zero based)
        // there can be only one digit from everything in every row/col/square        
        let valid (table : SudokuTable) (row,col) digit =         
            table.[row,col] = 0 && // not taken yet
            seq {         
            for c in 0 .. 8 do // in the same row
                yield table.[row, c] 
            for r in 0 .. 8 do // in the same col
                yield table.[r, col]
            let sr, sc = row / 3, col / 3
            for r in sr * 3 .. sr * 3 + 2 do // in the same square
                for c in sc * 3 .. sc * 3 + 2 do
                    yield table.[r, c] }
            |> Seq.exists ((=) digit) |> not // the given digit is not yet in the row, col or square
        
        // find the solution to a 2D array
        let solve table =             
            // if we use an array for convenient index based access, we lose immutability and easy backtracking
            // but it can set back manually after the recursive call ends. when a solution if found we can use an exception, or a choice            
            let table = table |> Array2D.copy // preserve the input table
            let size = table |> Array2D.length1
            let next (r,c) = // go from top left to bottom right one by one
                if c+1 >= size then (r+1,0) else (r,c+1)
            let rec loop table (r,c) = seq {
                if r >= size then // past the last row
                    //raise( SolutionException(table) ) // or return Choice1of2
                    yield table |> Array2D.copy // there is always a unique solution, but...
                else 
                    // try to fill the next cell
                    if table.[r,c] <> 0 then
                        yield! loop table ((r,c) |> next)
                    else // try to fill with any valid number
                        for d in 1 .. 9 do
                            if valid table (r,c) d then
                                // fill the table cell
                                table.[r,c] <- d                                    
                                yield! loop table ((r,c) |> next) // if this leads to a solution that will be yielded, then the original table mutated back
                                // if we are here there was no solution, so backtrack
                                table.[r,c] <- 0 }
            // start
            loop table (0,0) |> Seq.head

    assertEqual (Sudoku2.solve sudokuIn) (sudokuOut) "sudoku"
