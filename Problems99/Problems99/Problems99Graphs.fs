namespace Problems99

module Graphs = 
    open Problems99.Utils
    open Problems99.Lists

    type 'a Edge = 'a * 'a

    type 'a Graph = 'a list * 'a Edge list

    // list of edges only contain one version and that is with the lower letter in the first place
    let g = (['b';'c';'d';'f';'g';'h';'k'],[('b','c');('b','f');('c','f');('f','k');('g','h')])

    type 'a Node = 'a * 'a list

    type 'a AdjacencyGraph = 'a Node list

    let ga = [('b',['c'; 'f']); ('c',['b'; 'f']); ('d',[]); ('f',['b'; 'c'; 'k']); 
              ('g',['h']); ('h',['g']); ('k',['f'])]
    

    // Problem 80 : Conversions
    let graphToAdjancencyGraph ((v,e) : Graph<_>) : AdjacencyGraph<_> = 
        // start with empty list
        let ga = v |> List.map (fun x -> x,[]) |> Map.ofList 
        // iterate edges and add the back and forth. by putting ga in front it can infer the type in the expression
        (ga,e) ||> List.fold (fun ga (s,e) -> 
            ga |> Map.addItems [(s,e::ga.[s]); (e, s::ga.[e])])
        |> Map.toList |> List.map (fun (k,s) -> (k, s |> List.sort))

    assertEqual (graphToAdjancencyGraph g) ga "graphToAdjancencyGraph"


    let adjacencyGraphToGraph (nodes: AdjacencyGraph<_>) : Graph<_> = 
        // construct empty graph from all nodes
        let v = nodes |> List.map fst |> List.sort
        // collect available pairs
        let e = nodes |> List.collect (fun (v, ns) -> 
                                ns |> List.map (fun n -> 
                                    (min v n, max v n)))
                      |> List.sort |> dedup
        v,e

    assertEqual (adjacencyGraphToGraph ga) g "adjacencyGraphToGraph"


    // Problem 81: Path from one node to another one
    let paths a b (g: AdjacencyGraph<_>) = 
        let gm = g |> Map.ofList
        let rec loop path visited =
            // for each neighbour not yet visited see if we are there. the looping paths will not yield results
            [for n in gm.[List.head path] do 
                if n = b then // we arrived, the path is good
                    yield b::path |> List.rev
                elif visited |> Set.contains n |> not then
                    // try to go deeper on the path
                    yield! loop (n::path) (Set.add n visited)]
        loop [a] (Set.singleton a)

    let g81 = [(1,[2;3]);(2,[3]);(3,[4]);(4,[2]);(5,[6]);(6,[5])]

    assertEqual (paths 1 4 g81) [[1; 2; 3; 4]; [1; 3; 4]] "paths"
    assertEqual (paths 2 6 g81) [] "paths"


    // Problem 82: Cycle from a given node
    let cycle n (g: AdjacencyGraph<_>) = 
        // if we don't filter the starting node when it is the end but visited, we can use paths
        paths n n g |> List.filter (List.length >> ((<) 1))

    assertEqual (cycle 2 g81) [[2; 3; 4; 2]] "cycle"
    assertEqual (cycle 1 g81) [] "cycle"


    // Problem 83: Construct all spanning trees
    let spanningTrees (g: AdjacencyGraph<'a>) = 
        let gm = g |> Map.ofList
        let gcnt = g |> List.length
        let rec loop cnt (sm: Map<'a, 'a list>) = // sm is the spanning tree adjacency graph
            seq{// will yield complete trees only
                if cnt = gcnt then // we have everything node in the tree. return a canonized tree
                    yield sm |> Map.toSeq |> Seq.map (fun (n,ns) -> (n, ns |> List.sort)) |> List.ofSeq |> List.sort
                else 
                    // find nodes that we can connect to either pointing out or in
                    let unvisited n = sm |> Map.containsKey n |> not
                    // outgoing
                    for s in sm |> Map.toSeq |> Seq.map fst do
                        // unvisited neighbours
                        for n in gm.[s] |> List.filter unvisited do                            
                            // add the followed edge and the new node to the graph
                            let sm' = sm |> Map.add s (n::sm.[s]) |> Map.add n []
                            yield! loop (cnt+1) sm'
                    // incoming
                    for s in gm |> Map.toSeq |> Seq.map fst |> Seq.filter unvisited do
                        // is any neighbour in sm?
                        for n in gm.[s] |> List.filter (unvisited >> not) do
                            // add the incoming edge 
                            let sm' = sm |> Map.add s [n] 
                            yield! loop (cnt+1) sm'
                }
        // start at the first node
        let start = [(g |> List.head |> fst), []] |> Map.ofList
        loop 1 start |> Set.ofSeq |> Set.toList

    //spanningTrees [(1,[2;3]);(2,[3]);(3,[4]);(4,[2])]