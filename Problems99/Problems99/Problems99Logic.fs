namespace Problems99

module Logic = 
    open Problems99.Utils

    //46: Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed. 
    let and' a b = a && b
    let or' a b = a || b
    let nand' a b = not(and' a b)
    let nor' a b = not(or' a b)
    let xor' a b = nor' (and' a b) (nand' a b)
    let impl' a b = not(and' a (not b))
    let equ' a b = not(xor' a b)

    let table pred = 
        let states = [true;false]
        for a in states do
            for b in states do
                printfn "%b %b %b" a b (pred a b)

    //table (fun a b -> (and' a (or' a b)))


    //47: Truth tables for logical expressions (2). Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write the logical expression in the more natural way, as in the example: A and (A or not B). Define operator precedence as usual; i.e. as in Java. 
    // xor
    let (&|) a b = xor' a b
    // nand
    let (^&&) a b = nand' a b
    // nor
    let (^||) a b = nor' a b
    // impl
    let (|->) a b = impl' a b

    //table (fun a b -> (a && (a || not b)))
    //Generalize problem P47 in such a way that the logical expression may contain any number of logical variables. Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List. 
    let tablen n expr = 
        let rec ttab n = 
            match n with
            | 0 -> [[]]
            | _ -> [for tt in ttab (n-1) do
                            yield true::tt
                            yield false::tt]
        let tt = ttab n
        let tos bt = System.String.Join(" ", bt |> List.map string)
        tt |> List.iter (fun i -> printfn "%s %b" (i |> tos) (expr i))

    //tablen 3 (fun [a;b;c] -> a && (b || c) = a && b || a && c)


    //49: Gray codes.
    let rec gray n = 
        if n = 0 then 
            [""]
        else 
            let g = gray (n-1)
            let prefix b = List.map ((+) b)
            (prefix "0" g) @ (prefix "1" (g |> List.rev))

    assertEqual (gray 3) ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"] "gray"


    //50: Huffman codes.
    type FreqTree = // order by appearance
    | Leaf of int * char
    | Node of int * FreqTree * FreqTree
    with 
        member x.Freq = 
            match x with
            | Leaf (freq, _) -> freq
            | Node (freq, _,_) -> freq
    end

    let huffman freqs = 
        let pop s = // get minimum frequency element from set
            let ((_,n) as e) = Set.minElement s
            n, Set.remove e s
        // merge the two least frequent element into a node. the least freq goes left
        let rec merge (tree: Set<int * FreqTree>) = 
            if Set.count tree = 1 then
                tree.MinimumElement |> snd
            else
                let m1, tree = pop tree
                let m2, tree = pop tree
                let m12 = Node( m1.Freq + m2.Freq, m1, m2 )
                merge (Set.add (m12.Freq,m12) tree)
        //annotate leaves with freq so that Set behaves as a priority queue
        let leaves = freqs |> List.map (fun (s,f) -> (f,Leaf(f,s))) |> Set.ofList
        let tree = merge leaves
        //from top to bottom, going left is 0. wikipedia example is wrong
        let rec codes prefix tree = 
            seq {   match tree with
                    | Leaf(_,s) -> 
                        yield (s, prefix)
                    | Node(_, l, r) -> 
                        yield! codes (prefix+"0") l
                        yield! codes (prefix+"1") r }

        tree |> codes "" |> List.ofSeq |> List.sortBy fst
                                    
    
    assertEqual (huffman [('a',40);('b',35);('c',20);('d',5)]) 
                [('a', "0"); ('b', "11"); ('c', "101"); ('d', "100")] "huffman" 
    
    assertEqual (huffman [('a',45);('b',13);('c',12);('d',16);('e',9);('f',5)] ) 
                [('a', "0"); ('b', "101"); ('c', "100"); ('d', "111"); ('e', "1101");('f', "1100")] "huffman"  
    