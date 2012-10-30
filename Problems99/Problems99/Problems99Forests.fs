namespace Problems99

module Forests = 
    open Problems99.Utils

    /// A multiway tree is composed of a root element and a (possibly empty) set of successors which
    /// are multiway trees themselves. A multiway tree is never empty. The set of successor trees is
    /// sometimes called a forest.
    type 'a Tree = Node of 'a  * 'a Tree list

    ///                              (a)
    ///                            /  |  \
    ///                          (f) (c) (b)
    ///                           |      /  \
    ///                          (g)   (d)  (e)

    let tree = Node ('a', [
                        Node ('f', [Node ('g', [])]);
                        Node ('c', []);
                        Node ('b', [Node ('d', []); Node ('e', [])])
                        ] )

    // Problem 70C : Count the nodes of a multiway tree.
    let nodeCount tree = 
        let rec loop accu tree = 
            match tree with
            | Node(_, forest) -> List.fold loop (accu+1) forest
        loop 0 tree
              
    assertEqual (nodeCount tree) 7 "nodeCount"


    // Problem 70 : Tree construction from a node string.
    // afg^^c^bd^e^^^
    let stringToTree s = 
        let rec build data forest tokens cont = 
            match tokens with
            | [] -> failwith "could not parse"
            | '^'::rest -> // the current node has no more children, create node and continue in parent context
                (Node(data, forest |> List.rev), rest) |> cont
            | h::t -> // start of new child, collect the branch then continue from here
                build h [] t <| fun (branch,rest) -> // one branch is built, add it to the forest of the current root (data) and look for other children
                    build data (branch::forest) rest cont
        let tokens = s |> List.ofSeq
        build (List.head tokens) [] (List.tail tokens) fst

    assertEqual (stringToTree "afg^^c^bd^e^^^") tree "stringToTree"


    // Problem 71 : Determine the internal path length of a tree.
    let internalPathLength tree = 
        let rec loop dist accu tree = 
            match tree with
            | Node(_, forest) ->
                forest |> List.fold (loop (dist+1)) (accu+dist)
        loop 0 0 tree

    assertEqual (internalPathLength tree) 9 "internalPathLength"


    // Problem 72 : Construct the bottom-up order sequence of the tree nodes.
    // gfcdeba
    let bottomUp tree = 
        let rec loop tree = 
            match tree with 
            | Node(d, forest) ->
                System.String.Join("", forest |> List.map loop) + d.ToString()
        loop tree

    assertEqual (bottomUp tree) "gfcdeba" "bottomUp"


    // Problem 73 : Lisp-like tree representation.
    // "(x (a (f g) c (b d e)))" -> ['('; 'x'; '('; 'a'; '('; 'f'; 'g'; ')'; 'c'; '('; 'b'; 'd'; 'e'; ')'; ')'; ')']
    let lispyTokenList s = 
        s |> List.ofSeq |> List.filter ((<>) ' ')

    assertEqual (lispyTokenList "(x (a (f g) c (b d e)))") ['('; 'x'; '('; 'a'; '('; 'f'; 'g'; ')'; 'c'; '('; 'b'; 'd'; 'e'; ')'; ')'; ')'] "lispyTokenList"

    
    let treeToLisp tree = 
        let rec loop tree = 
            match tree with
            | Node(d, []) -> d.ToString()
            | Node(d, forest) -> sprintf "(%s %s)" (d.ToString()) (System.String.Join(" ",(forest |> List.map loop)))
        loop tree

    assertEqual (treeToLisp tree) "(a (f g) c (b d e))" "treeToLisp"


    let lispToTree s = 
        let rec build data forest tokens cont = 
            match tokens with
            | [] -> failwith "could not parse"
            | ')'::rest -> // end of branch
                cont (Node(data, forest |> List.rev), rest) 
            | '('::head::rest -> // beginning of lower branch
                build head [] rest <| fun (branch, rest) -> // parse the child branch
                    build data (branch::forest) rest cont // continue parsing the parent node with appending the branch to the forest
            | head::rest -> // single node child branch
                build data (Node(head,[])::forest) rest cont

        match s |> lispyTokenList with
        | '('::head::rest -> build head [] rest fst // start in the first root branch
        | data::[] -> Node(data, []) 
        | _ -> failwith "could not parse"

    assertEqual (lispToTree "(a (f g) c (b d e))") tree "lispToTree"