namespace Algorithms.Treemaps


// http://www.win.tue.nl/~vanwijk/stm.pdf

module Squarified = 

    type Division = 
        | Vertical // above each other
        | Horizontal // next to each other
        with
        member this.Other = 
            match this with 
            | Vertical -> Horizontal
            | Horizontal -> Vertical


    type Rectangle = { Width: double; Height: double } with

        member this.Area = this.Height * this.Width

        member this.Aspect = 
            if this.Height > this.Width then
                this.Height / this.Width
            else
                this.Width / this.Height 

        /// Preferred division type
        member this.Division = 
            if this.Height > this.Width then
                Vertical
            else
                Horizontal

        /// Slice the rectangle into two in the preferred way.
        member this.Divide area = 
            match this.Division with 
                | Horizontal -> { Height = this.Height; Width  = area / this.Height }
                | Vertical   -> { Width  = this.Width;  Height = area / this.Width }



    /// Associate an area with a piece of data
    type 'a Item = double * 'a 
    type 'a Stack = (Rectangle * 'a Item) list

    type 'a Treemap = 
        | Branch of Rectangle * 'a Treemap * 'a Treemap // subdivision of a rectangle into a terminal part and another map        
        | Leaf of Rectangle * 'a Stack // the list of terminal items in a subdivision
        | Empty of Rectangle // no items to fill it        


    let area (item: 'a Item) = 
        fst item

    /// Given a list of laid out items try to stack another one at the top, 
    /// recalculating the layout of each item to form a new common rectangle.
    let stack (rect: Rectangle) (item: 'a Item) (stck: 'a Stack) = 
        // if we want to divide the rectangle horizontally, 
        // we are going to try to stack the items on the left side vertially
        let direction = rect.Division.Other
        let size = match direction with 
            | Vertical -> rect.Height
            | Horizontal -> rect.Width

        match stck with
        | [] -> 
            // create the first item to fill the space
            let rect = 
                match direction with
                | Vertical   -> { Height = size; Width = (area item) / size }
                | Horizontal -> { Width = size; Height = (area item) / size }
            [rect, item]

        | items -> 
            // calculate the total area of the items and create new rectangles
            let total = (area item) + (stck |> List.sumBy (fun patch -> (fst patch).Area))
            // other dimension depending on division and total area
            let other = total / size
            let items = item :: (items |> List.map snd)
            let stck = items |> List.map (fun item ->
                let rect = 
                    match direction with
                    | Vertical   -> { Width = other; Height = (area item) / other }
                    | Horizontal -> { Height = other; Width = (area item) / other }
                rect, item)
            stck


    /// Stack items as long as the aspect ration gets better (closer to 1).
    let stackItems rect (items: 'a Item list) = 
        let push = stack rect
        let rec loop items (stck: 'a Stack) = 
            match items with 
            | [] -> stck, []
            | item :: rest -> 
                let stck' = stck |> push item
                if stck |> List.isEmpty then
                    loop rest stck'
                else
                    let top  = stck  |> List.head |> fst
                    let top' = stck' |> List.head |> fst
                    if top'.Aspect > top.Aspect then
                        stck, items
                    else
                        loop rest stck'
        loop items []
                    


    /// Take a list of items and a target rectangle and return a treemap.
    let layout rect items = 
        
        // recursively divide the rectangle into subdivisons and stuff items
        let rec loop (rect: Rectangle) items cont =   
            match items with
            | [] -> 
                Empty rect |> cont
            | items ->                          
                // stack items as long as the aspect ratio gets better
                let stck, rest = items |> stackItems rect
                // combine the stack into one leaf node and fill the other empty part with the rest
                let total = stck |> List.sumBy (fun patch -> (fst patch).Area)
                let srect = rect.Divide total
                let erect = rect.Divide (rect.Area - total)     
            
                loop erect rest <| fun map ->
                    Branch (rect, 
                        Leaf (srect, stck), 
                        map) |> cont
        
        // descending order gives best results
        let items = items |> List.sort |> List.rev
        loop rect items id



module Tests = 
    
    open NUnit.Framework
    open Squarified
    open System

    [<Test>]
    let StackToEmptyGivesRightSize() = 
        let rect = {Width = 6.0; Height = 4.0}
        let stck = stack rect (6.0, 6) [] 
        let exp  = [{ Height = 4.0; Width = 6.0/4.0}, (6.0, 6) ]

        Assert.AreEqual(exp, stck)


    [<Test>]
    let StackToExistingGivesRightSize() = 
        let rect = {Width = 6.0; Height = 4.0}
        let push = stack rect
        let stck = [] |> push (6.0, 6) |> push (6.0, 6) |> push (4.0, 4) 
        let exp  = [
            { Height = 1.0; Width = 4.0}, (4.0, 4);
            { Height = 1.5; Width = 4.0}, (6.0, 6);
            { Height = 1.5; Width = 4.0}, (6.0, 6); ]

        Assert.AreEqual(exp, stck)


    [<Test>]
    let StackStopsWhenAspectGetsWorse() = 
        let rect = {Width = 6.0; Height = 4.0}
        let stck, rest = [(6.0, "6.a");(6.0, "6.b");(4.0, "4")] |> stackItems rect
        let exp  = [
            { Height = 2.0; Width = 3.0}, (6.0, "6.b");
            { Height = 2.0; Width = 3.0}, (6.0, "6.a"); ]

        Assert.AreEqual(exp, stck)

        Assert.AreEqual(rest, [(4.0, "4")])


    [<Test>]
    let LayoutEmptyGivesEmptyMap() = 
        let rect = {Height = 4.0; Width = 6.0}
        let (map: int Treemap) = layout rect []
        let (exp: int Treemap) = Empty rect

        Assert.AreEqual(exp, map)


    [<Test>]
    let LayoutItemsAccordingToExampleWorks() =             
        let items = [6;6;4;3;2;2;1] |> List.map (fun i -> (double i, i))

        (*
        666666 22 22 1
        666666 22 22 1
        666666 22 22 1
               444 33
        666666 444 33
        666666 444 33
        666666 444 33
        *)

        let map = layout {Width = 6.0; Height = 4.0} items

        let (|Rect|_|) w h (rect: Rectangle) = 
            if rect.Width = w && rect.Height = h then Some() else None

        let (|Item|_|) i item = 
            if item = (double i, i) then Some() else None

        let (|Aspect|_|) a b (rect: Rectangle) = 
            let aspect = (double a)/(double b)
            if Math.Abs(rect.Aspect - aspect) < 0.01 then Some() else None

        let good = 
            match map with
                | Branch (Rect 6.0 4.0, 
                            Leaf (Rect 3.0 4.0,
                                [Aspect 3 2, Item 6; 
                                 Rect 3.0 2.0, Item 6]),
                            Branch (Rect 3.0 4.0, 
                                    Leaf (_, 
                                         [Aspect 49 27, Item 3;
                                          _, Item 4]),
                                    Branch (_, 
                                            Leaf (_, 
                                                 [Aspect 25 18, Item 2]),
                                            Branch (_, 
                                                    Leaf (Aspect 25 18, 
                                                         [_, Item 2]),
                                                    Branch (_, 
                                                         Leaf (Aspect 25 9, 
                                                                [_, Item 1]),
                                                         Empty _)))))
                    -> true
                | _ -> false
        Assert.IsTrue(good)
            



