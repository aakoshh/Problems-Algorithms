namespace Problems99

module Trees = 
    open Problems99.Utils

    //54A: Check whether a given term represents a binary tree 
    type 'a Tree = Empty | Branch of 'a * 'a Tree * 'a Tree
    
    let leaf x = Branch(x, Empty, Empty)


    //55: Construct completely balanced binary trees 
    // generate all possible balanced btrees of n nodes
    let cbalTree n = 
        let rec build = memoize(fun n ->
            if n = 0 then
                [Empty]
            else // half the tree and combine the possibilities of the two halves
                let h1 = (n-1)/2 
                let h2 = (n-1) - h1
                [for t1 in build h1 do
                    for t2 in build h2 do
                        yield Branch('x', t1, t2)
                        if h1 <> h2 then // if equal then the two permutations would be identical
                            yield Branch('x', t2, t1)])
        build n

    assertEqual ([0;1;2;3;4] |> List.map cbalTree |> List.map List.length) [1; 1; 2; 1; 4] "cbalTree"


    //56: Symmetric binary trees 
    let symmetric tree = 
        let rec symmetric' t1 t2 = 
            match t1, t2 with
            | Empty, Empty -> true
            | Branch(_, t1l, t1r), Branch(_, t2l, t2r) -> 
                (symmetric' t1l t2r) && (symmetric' t1r t2l)
            | _ -> false
        symmetric' tree tree

    // with continuation
    let symmetric2 tree = 
        let rec symmetric' t1 t2 cont = 
            match t1, t2 with
            | Empty, Empty -> cont true
            | Branch(_, t1l, t1r), Branch(_, t2l, t2r) -> // compare left with right
                symmetric' t1l t2r (fun isLeft ->
                    isLeft && symmetric' t1r t2l cont //(fun isRight -> cont (isLeft && isRight))
                    )
            | _ -> cont false // branch against empty
        symmetric' tree tree id

    assertEqual (symmetric2 (Branch('x', Empty, Empty))) true "symmetric"
    assertEqual (symmetric2 (Branch('x', Branch('x',Empty,Empty), Empty))) false "symmetric"
    assertEqual (symmetric2 (Branch ('x', Branch ('x', Empty, Empty), Branch ('x', Empty, Empty)))) true "symmetric"
        

    //57: Binary search trees (dictionaries) 
    let rec insert tree x = 
        match tree with
        | Empty -> Branch(x, Empty, Empty)
        | Branch(y, lt, rt) ->
            if   x > y then Branch(y, lt, (insert rt x))
            elif x < y then Branch(y, (insert lt x), rt)
            else tree
    
    // with continuations
    let insert2 tree x = 
        let rec insert' tree x cont = 
            match tree with
            | Empty -> cont (Branch(x, Empty, Empty))
            | Branch(y, lt, rt) ->
                if x > y then 
                    insert' rt x (fun nrt -> cont (Branch(y, lt, nrt)))
                elif x < y then 
                    insert' lt x (fun nlt -> cont (Branch(y, nlt, rt)))
                else cont tree
        insert' tree x id

    let construct lst = 
        lst |> List.fold (fun tree x -> insert2 tree x) Empty

    assertEqual ([5; 3; 18; 1; 4; 12; 21] |> construct |> symmetric) true "construct"
    assertEqual ([3; 2; 5; 7; 4] |> construct |> symmetric) false "construct"


    //58: Generate-and-test paradigm 
    let symCbalTrees n = 
        n |> cbalTree |> List.filter symmetric

    assertEqual (symCbalTrees 5 |> List.length) 2 "symCbalTrees"


    //59: Construct height-balanced binary trees 
    //     /\
    //    /\ \
    //     /
    let hbalTree h = 
        let rec build = memoize(fun n ->
            if n = 0 then
                [(0,Empty)]
            elif n = 1 then
                [(1,Branch('x',Empty,Empty))]
            else // try to build smaller trees and return those that can be combined
                // try to combine h-1 and h-2 height trees
                let trees = (build (n-1)) @ (build (n-2))
                [for hl,tl in trees do
                    for hr,tr in trees do
                        let h = 1+(max hl hr)
                        if h = n then // not n-2 both
                            yield (n, Branch('x', tl, tr))] )
        build h |> List.map snd

    //min height: to build a height of 5, add one root to the minimum 4 and 3
    assertEqual (3 |> hbalTree |> Seq.take 4 |> List.ofSeq)
                [Branch ('x',Branch ('x',Branch ('x',Empty,Empty),Branch ('x',Empty,Empty)),
                             Branch ('x',Branch ('x',Empty,Empty),Branch ('x',Empty,Empty)));
                  Branch ('x',Branch ('x',Branch ('x',Empty,Empty),Branch ('x',Empty,Empty)),
                              Branch ('x',Branch ('x',Empty,Empty),Empty));
                  Branch ('x',Branch ('x',Branch ('x',Empty,Empty),Branch ('x',Empty,Empty)),
                              Branch ('x',Empty,Branch ('x',Empty,Empty)));
                  Branch ('x',Branch ('x',Branch ('x',Empty,Empty),Branch ('x',Empty,Empty)),
                              Branch ('x',Empty,Empty))] "hbalTree"
    

    //60: Construct height-balanced binary trees with a given number of nodes Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain? Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This question is more difficult. Try to find a recursive statement and turn it into a function minNodes that returns the minimum number of nodes in a height-balanced binary tree of height H.
    let hbalMaxNodes h = 
        if h = 0 then 0 else (pown 2 h) - 1

    let rec hbalMinNodes = memoize(fun h ->
        match h with
        | 0 -> 0
        | 1 -> 1
        | _ -> 1 + (hbalMinNodes (h-1)) + (hbalMinNodes (h-2)))

    assertEqual ([1;2;3;4;5] |> List.map hbalMinNodes) [1;2;4;7;12] "hbalMinNodes"

    let hbalMinHeight n = 
        1 + int(System.Math.Log((float n), 2.0))

    let hbalMaxHeight n = 
        // if min nodes for height h is n, then for n nodes the max height is h
        let rec loop h = 
            let nmin = hbalMinNodes h
            if nmin > n then h-1 else loop (h+1)
        loop 0
    
    assertEqual ([0..12] |> List.map hbalMaxHeight) [0;1;2;2;3;3;3;4;4;4;4;4;5] "hbalMinNodes"

    // generic function to traverse the structure of a tree using continuations
    // pass the value of the empty node and how to combine results of left and right
    let traverse empty f = 
        fun tree ->
            let rec loop tree cont = 
                match tree with 
                | Empty -> cont empty
                | Branch(_, tl, tr) -> loop tl <| fun hl -> loop tr <| fun hr -> cont (f hl hr)
            loop tree id


    let height tree = tree |> traverse 0 (fun l r -> 1+(max l r))

    assertEqual (height (Branch('x',Branch('x',Empty,Empty),Branch('x',Empty,Empty)))) 2 "height"

    
    let nodes tree = tree |> traverse 0 (fun l r -> 1+l+r)

    assertEqual (nodes (Branch('x',Branch('x',Empty,Empty),Branch('x',Empty,Empty)))) 3 "nodes"

    
    let hbalTreeNodes n = 
        let hmax = hbalMaxHeight n
        let hmin = hbalMinHeight n
        seq{ for i in hmin .. hmax do
                yield! hbalTree i }
            |> Seq.filter (fun t -> nodes t = n) |> List.ofSeq

    assertEqual (hbalTreeNodes 15 |> List.length) 1553 "hbalTreeNodes"


    //61: Count the leaves of a binary tree
    let foldTree accu f tree = 
        let rec loop accu tree cont = 
            match tree with 
            | Empty -> cont accu
            | Branch(_, tl, tr) as node ->
                let accu = (f accu node)
                loop accu tl <| fun accu -> loop accu tr <| fun accu -> cont accu
        loop accu tree id

    // fold tree by a three parameter function that combines the data with the left and right accumulators
    let foldTree3 empty f tree = 
        let rec loop tree cont = 
            match tree with
            | Empty -> cont empty
            | Branch(d, lt, rt) ->
                loop lt <| fun lacc ->
                    loop rt <| fun racc ->
                        f d lacc racc |> cont
        loop tree id

    let leafCount tree = 
        tree |> foldTree 0 (fun accu node ->
                                match node with 
                                | Branch(_,Empty,Empty) -> accu + 1
                                | _ -> accu)

    assertEqual (leafCount (Branch (1, Branch (2, Empty, Branch (4, Empty, Empty)),
                                       Branch (3, Empty, Empty)))) 2 "leafCount"


    //61A: Collect the leaves of a binary tree in a list
    let leafList tree = 
        tree |> foldTree [] (fun accu node ->
                                match node with 
                                | Branch(v,Empty,Empty) -> v::accu
                                | _ -> accu) |> List.sort

    assertEqual (leafList (Branch (1, Branch (2, Empty, Branch (4, Empty, Empty)),
                                      Branch (3, Empty, Empty))))
                [3;4] "leafList"


    //62: Collect the internal nodes of a binary tree in a list An internal node of a binary tree has either one or two non-empty successors. Write a predicate internals/2 to collect them in a list.
    let internalList tree = 
        tree |> foldTree [] (fun accu node ->
                                match node with 
                                | Empty | Branch(_,Empty,Empty) -> accu
                                | Branch(v,_,_) -> v::accu) |> List.sort

    assertEqual (internalList (Branch (1, Branch (2, Empty, Branch (4, Empty, Empty)),
                                          Branch (3, Empty, Empty))))
                [1;2] "internalList"


    //62B: Collect the nodes at a given level in a list A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. Write a predicate atlevel/3 to collect all nodes at a given level in a list.
    let nodesAt n tree = 
        let rec loop accu n tree cont = 
            if n = 0 then 
                cont accu
            else 
                match tree with 
                | Empty -> cont accu
                | Branch(v, tl, tr) as node ->
                    let accu = if n = 1 then v::accu else accu
                    loop accu (n-1) tl <| fun accu -> loop accu (n-1) tr <| cont
        loop [] n tree id |> List.sort

    assertEqual (nodesAt 2 (Branch (1, Branch (2, Empty, Branch (4, Empty, Empty)),
                                          Branch (3, Empty, Empty))))
                [2;3] "nodesAt"



    //63: Construct a complete binary tree
    let completeBinaryTree n = 
        // has the heap property so each child node will have address based on parent, contiguously
        let rec build addr cont = 
            if addr > n then
                cont Empty
            else 
                build (2*addr) <| fun lt -> build (2*addr+1) <| fun rt -> cont (Branch(addr, lt, rt))
        build 1 id

    assertEqual (completeBinaryTree 4) (Branch (1,Branch (2,Branch (4,Empty,Empty),Empty),Branch (3,Empty,Empty))) "completeBinaryTree"


    let isCompleteBinaryTree tree = 
        //enumerate and check addresses
        let n = tree |> nodes
        let rec check tree addr cont = 
            match addr, tree with
            | a, Empty when a <= n -> cont false // should be a branch, as in the constructing function
            | a, Branch(_,_,_) when a > n -> cont false // should be empty
            | a, Empty when a > n -> cont true // terminated
            | a, Branch(_, lt, rt) when a <= n -> // check the left and right part
                 check lt (2*addr) <| fun clt -> check rt (2*addr+1) <| fun crt -> cont (clt && crt) 
        check tree 1 id

    assertEqual (isCompleteBinaryTree (Branch (1,Branch (2,Branch (4,Empty,Empty),Empty),Branch (3,Empty,Empty)))) true "isCompleteBinaryTree"
    assertEqual (isCompleteBinaryTree (Branch (1,Branch (2,Empty,Branch (4,Empty,Empty)),Branch (3,Empty,Empty)))) false "isCompleteBinaryTree"


    //64: Drawing Binary Trees
    ///     1  2  3  4  5  6  7  8  9  10  11  12
    /// 
    /// 1                       (n)
    ///                       /             \
    /// 2                 (k)                  (u)
    ///             /        \           /
    /// 3     (c)            (m)   (p)
    ///      /     \                    \
    /// 4  (a)         (h)                 (s)
    ///               /                   /
    /// 5           (g)                (q)
    ///            /
    /// 6        (e)
    // return tree with each node annotated with vertical (x) and horizontal (y) coordinates
    let layout tree = 
        let rec build tree depth maxx cont = 
            match tree with 
            | Empty -> cont maxx Empty
            | Branch(d, tl, tr) ->
                // build the left tree then knowing maxx build the right and add the middle node
                build tl (depth+1) maxx <| fun maxxl tl -> 
                    build tr (depth+1) (maxxl+1) <| fun maxxr tr ->
                        (maxxr,Branch( (d, (maxxl+1, depth)), tl, tr)) ||> cont
        build tree 1 0 (fun maxx tree -> tree)


    let tree64 = Branch ('n',
                     Branch ('k',
                            Branch ('c',
                                    Branch ('a', Empty, Empty),
                                    Branch ('h',
                                            Branch ('g',
                                                    Branch ('e', Empty, Empty),
                                                    Empty),
                                            Empty)
                                    ),
                            Branch ('m', Empty, Empty)),
                     Branch ('u',
                            Branch ('p',
                                    Empty,
                                    Branch ('s',
                                            Branch ('q', Empty, Empty),
                                            Empty)
                                    ),
                            Empty))

    assertEqual (layout tree64)
                (Branch(('n', (8, 1)),
                          Branch
                            (('k', (6, 2)),
                             Branch
                               (('c', (2, 3)),Branch (('a', (1, 4)),Empty,Empty),
                                Branch
                                  (('h', (5, 4)),
                                   Branch (('g', (4, 5)),Branch (('e', (3, 6)),Empty,Empty),Empty),
                                   Empty)),Branch (('m', (7, 3)),Empty,Empty)),
                          Branch
                            (('u', (12, 2)),
                             Branch
                               (('p', (9, 3)),Empty,
                                Branch (('s', (11, 4)),Branch (('q', (10, 5)),Empty,Empty),Empty)),
                             Empty))) "layout"


    //65: Drawing Binary Trees (2)
    ///     1  2  3  4  5  6  7  8  9  10  11  12  13  14  15  16  17  18  19  20  21  22  23
    /// 
    /// 1                                                  (n)
    ///                                        /                               \
    /// 2                     (k)                                                          (u) 
    ///                  /            \                                              /
    /// 3        (c)                       (m)                             (p)
    ///       /       \                                                         \
    /// 4  (a)         (e)                                                         (q)
    ///               /   \
    /// 5          (d)    (g)

    // On a given level, the horizontal distance between neighboring nodes is constant.

    // apply a transformation function to node data
    let mapTree f tree = 
        let rec loop tree cont = 
            match tree with 
            | Empty -> cont tree
            | Branch(d, tl, tr) -> 
                loop tl <| fun tl -> loop tr <| fun tr -> Branch(f d, tl, tr) |> cont
        loop tree id
        
    // layout so that the parent is in the middle between its children
    let layoutEven tree = 
        let maxlvl = height tree
        let rec loop tree lvl idx cont = 
            match tree with 
            | Empty -> cont Empty
            | Branch(d, tl, tr) ->
                // on level x one child is max x-1 high which would contain max 2^(x-1)-1 nodes
                // one of which would be the child, half of the rest could go into either branch.
                // that says the empty indices between parent and child is 0, 1, 3, 7, etc corresponding to
                // 2^0-1, 2^1-1, 2^2-1, 2^3-1
                let space = (pown 2 (maxlvl - lvl - 1)) - 1 
                // start laying out at zero and go negative and positive,
                loop tl (lvl+1) (idx-1-space) <| fun tl ->
                    loop tr (lvl+1) (idx+1+space) <| fun tr -> 
                        Branch( (d, (idx, lvl)), tl, tr) |> cont
        let ntree = loop tree 1 0 id
        // what's the minimum?
        let minx = ntree |> foldTree 0 (fun accu node ->
                        match node with 
                        | Empty -> accu
                        | Branch( (_,(x,_)), _, _) -> min accu x)
        // subtract the minimum
        ntree |> mapTree (fun (d, (x,y)) -> (d, (x-minx+1, y)))


    let tree65 = Branch ('n',
                        Branch ('k',
                                Branch ('c',
                                        Branch ('a', Empty, Empty),
                                        Branch ('e',
                                                Branch ('d', Empty, Empty),
                                                Branch ('g', Empty, Empty))
                                        ),
                                Branch ('m', Empty, Empty)),
                        Branch ('u',
                                Branch ('p',
                                        Empty,
                                        Branch ('q', Empty, Empty)),
                                Empty)) 
    
    assertEqual (layoutEven tree65) 
                   (Branch
                     (('n', (15, 1)),
                          Branch
                            (('k', (7, 2)),
                             Branch
                               (('c', (3, 3)),Branch (('a', (1, 4)),Empty,Empty),
                                Branch
                                  (('e', (5, 4)),Branch (('d', (4, 5)),Empty,Empty),
                                   Branch (('g', (6, 5)),Empty,Empty))),
                             Branch (('m', (11, 3)),Empty,Empty)),
                          Branch
                            (('u', (23, 2)),
                             Branch (('p', (19, 3)),Empty,Branch (('q', (21, 4)),Empty,Empty)),
                             Empty))) "layoutEven"


    // 66: Drawing Binary Trees (3)
    ///     1  2  3  4  5  6  7  
    /// 
    /// 1              (n) 
    ///              /     \
    /// 2        (k)         (u)
    ///         /   \       /
    /// 3     (c)   (m)   (p)
    ///       /  \          \    
    /// 4  (a)   (e)         (q)
    ///          /   \
    /// 5     (d)    (g)

    let layoutLean tree = 
        // transform the tree so that each node has a list representing the left/right sway under it
        let sway = function
            | Empty -> [] 
            | Branch( (_,sl), _,_) -> sl
        // given two sway lists construct a merged for each level below
        let rec mergeSway sl sr accu = 
            match sl, sr with
            | [],[] -> accu |> List.rev
            | (ll,lr)::lt, [] -> 
                mergeSway lt [] ((ll-1,lr-1)::accu)
            | [], (rl,rr)::rt -> 
                mergeSway [] rt ((rl+1,rr+1)::accu)
            | (ll,lr)::lt, (rl,rr)::rt ->
                mergeSway lt rt ((min (ll-1) (rl+1), max (lr-1) (rr+1))::accu)
        // given two sway lists find the least distance the branches can be from each other
        let rec swayDist sl sr dx = 
            match sl, sr with
            | [], _ | _, [] -> dx
            | (ll,lr)::lt, (rl,rr)::rt ->
                // if the left overlaps right side grow distance
                let dx = if (lr - dx) >= (rl + dx) then dx + 1 + ((lr - dx) - (rl + dx)) else dx
                swayDist lt rt dx
        // build a tree with each node having a list of sway per depth under it
        let rec buildSway tree cont = 
            match tree with 
            | Empty -> cont Empty
            | Branch(d, lt, rt) ->
                buildSway lt <| fun lt ->
                    buildSway rt <| fun rt ->
                        // how much did the left and right sway relative to the parent
                        // defaults will be corrected by merge to zero, the list will always start with (0,0)
                        let sl = (1,1)::(sway lt)
                        let sr = (-1,-1)::(sway rt)
                        let s  = mergeSway sl sr []
                        Branch( (d, s), lt, rt) |> cont
        let stree = buildSway tree id
        // now that we know the level-by-level sways merge the branches
        let rec layoutBySway tree lvl idx cont = 
            match tree with
            | Empty -> cont Empty
            | Branch((d,_), lt, rt) ->
                let dx = swayDist (sway lt) (sway rt) 1 // try the default distance
                // lay out the left side relative to the root, we will shift it later
                layoutBySway lt (lvl+1) (idx-dx) <| fun lt ->
                    layoutBySway rt (lvl+1) (idx+dx) <| fun rt ->
                        Branch( (d, (idx,lvl)), lt, rt ) |> cont
        let ntree = layoutBySway stree 1 0 id
        // what's the minimum?
        let minx = ntree |> foldTree 0 (fun accu node ->
                        match node with 
                        | Empty -> accu
                        | Branch( (_,(x,_)), _, _) -> min accu x)
        // subtract the minimum
        ntree |> mapTree (fun (d, (x,y)) -> (d, (x-minx+1, y))) 
                        

    assertEqual (layoutLean tree65) 
            (Branch
                 (('n', (5, 1)),
                  Branch
                    (('k', (3, 2)),
                     Branch
                       (('c', (2, 3)),Branch (('a', (1, 4)),Empty,Empty),
                        Branch
                          (('e', (3, 4)),Branch (('d', (2, 5)),Empty,Empty),
                           Branch (('g', (4, 5)),Empty,Empty))),
                     Branch (('m', (4, 3)),Empty,Empty)),
                  Branch
                    (('u', (7, 2)),
                     Branch (('p', (6, 3)),Empty,Branch (('q', (7, 4)),Empty,Empty)),Empty))) 
                 "layoutLean"
                        
    
    // 67: A string representation of binary trees
    // a(b(d,e),c(,f(g,))) 
    let treeToString tree =
        let rec concat tree cont = 
            match tree with
            | Empty -> cont ""
            | Branch(d, Empty, Empty) -> d.ToString() |> cont
            | Branch(d, lt, rt) ->
                concat lt <| fun lts ->
                    concat rt <| fun rts ->
                        sprintf "%s(%s,%s)" (d.ToString()) lts rts |> cont 
        concat tree id

    let tree67 = Branch ("x",Branch ("y",Empty,Empty),Branch ("a",Empty,Branch ("b",Empty,Empty)))

    assertEqual (treeToString tree67) "x(y,a(,b))" "treeToString"


    let stringToTree (str: string) = 
        let rec parse (str: string) (cont: string Tree -> string -> string Tree) = 
            // find the first ( or , or )
            let ipl = str.IndexOf('(')
            let icm = str.IndexOf(',')
            let ipr = str.IndexOf(')')
            let ifs = str.IndexOfAny([|'(';',';')'|])
            if ifs < 0 && str <> "" then
                cont (Branch(str,Empty,Empty)) ""
            elif ifs >= 0 && ifs = ipl then // must be a branch
                let d = str.Substring(0,ifs)                
                let rest = str.Substring(ifs+1) 
                parse rest <| fun lt rest -> // parsed the left part
                    parse rest <| fun rt rest -> // parsed the right part
                        let rest = if rest.StartsWith(",") then rest.Substring(1) else rest // go to next branch. after right is parsed the , must be the next branch
                        cont (Branch(d, lt, rt)) rest // combine the parsed tree
            elif ifs >= 0 && (ifs = icm || ifs = ipr) then // if no ( then must be a branch without children            
                let d = str.Substring(0,ifs)
                let rest = str.Substring(ifs+1)
                let node = if d = "" then Empty else Branch(d,Empty,Empty)
                cont node rest // this side of the tree is parsed, continue in the above part
            else // empty string, should not happen because what could we pass to cont?
                invalidArg "str" "could not parse"
        parse (str.Replace(" ","")) (fun tree rest -> tree)

    assertEqual ("x(y,a(,b))" |> stringToTree) tree67 "stringtoTree"
    assertEqual ("a(b(d,e),c(,f(g,)))" |> stringToTree |> treeToString) "a(b(d,e),c(,f(g,)))" "stringToTree"
    assertEqual ("a(b(d,e),c(h(i,j),f(g,)))" |> stringToTree |> treeToString) "a(b(d,e),c(h(i,j),f(g,)))" "stringToTree"

    // 68: Preorder and inorder sequences of binary trees.
    let tree68 = "a(b(d,e),c(,f(g,)))" |> stringToTree 

    let preOrder' tree = 
        tree |> foldTree [] (fun accu node ->
                            match node with
                            | Empty -> accu
                            | Branch(d, _, _) -> d::accu)
        |> List.rev |> (fun lst -> System.String.Join("", lst))

    let preOrder tree = 
        tree |> foldTree3 "" (fun d l r -> d+l+r) 

    assertEqual (tree68 |> preOrder') "abdecfg" "preOrder"
    assertEqual (tree68 |> preOrder) "abdecfg" "preOrder"

    // the root will be in the middle
    let inOrder tree = 
        tree |> foldTree3 "" (fun d l r -> l+d+r) 

    assertEqual (tree68 |> inOrder) "dbeacgf" "inOrder"


    // given pre and in order construct tree
    let stringToTree2 pos ios = 
        // first of pos is the root
        // the root separates its left and right children in ios
        let rec build (pos:string) (ios: string) cont = 
            if pos = "" then 
                cont Empty 
            else
                let root = pos.[0] |> string
                let ridx = ios.IndexOf(root) // everything up to that is the left child
                let iol = if ridx > 0 then ios.Substring(0, ridx) else ""
                let ior = if ridx < ios.Length-1 then ios.Substring(ridx+1) else ""
                //printfn "pos=%s ios=%s root=%s ridx=%d iol=%s ior=%s" pos ios (string root) ridx iol ior
                // the pre-order has the same number of children encoded
                let pol = if iol.Length > 0 then pos.Substring(1,iol.Length) else ""
                let por = if ior.Length > 0 then pos.Substring(1+iol.Length, ior.Length) else ""
                // consturct the left, then the right, then create the branch
                build pol iol <| fun lt ->
                    build por ior <| fun rt ->
                        Branch(root, lt, rt) |> cont
        build pos ios id
        

    assertEqual (stringToTree2 "abdecfg" "dbeacgf") tree68 "stringToTree2"


    // Problem 69 : Dotstring representation of binary trees.
    // a(b(d,e),c(,f(g,))) -> abd..e..c.fg...
    let treeDotString tree = 
        tree |> foldTree3 "." (fun d l r -> d+l+r) 

    assertEqual (tree68 |> treeDotString) "abd..e..c.fg..." "treeDotString"

    // parse dotstring format
    let dotStringToTree (s: string) = 
        let rec parse (s: char list) cont = 
            match s with 
            | [] -> failwith "could not parse" // should not be here
            | h::rest when h = '.' -> cont Empty rest // continue with parsing the rest where we were
            | h::rest -> // data branch
                parse rest <| fun lt rest -> // parse the left side until dots are reached
                    parse rest <| fun rt rest ->  // thre right side needs dots as well to terminate
                        cont (Branch(string h, lt, rt)) rest // add a branch and continue in parent continuation with the rest of the string
        parse (s |> List.ofSeq) (fun tree rest -> tree)

    assertEqual (dotStringToTree "abd..e..c.fg...") tree68 "dotStringToTree"