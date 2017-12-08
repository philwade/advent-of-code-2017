module DayThree

type Direction =
    | North
    | South
    | East
    | West
    | Start

type Axis =
    | X
    | Y

let diffLookup (x, y) =
    (abs x) + (abs y)

let addValues acc axis valuelist coordlist axisconst =
    let pairs = List.zip valuelist coordlist
    match axis with
        | X ->
            List.fold (fun x (value, coord) ->
                        Map.add value (coord, axisconst) x
                        ) acc pairs
        | Y ->
            List.fold (fun x (value, coord) ->
                        Map.add value (axisconst, coord) x
                        ) acc pairs

let addValues2 acc axis valuelist coordlist axisconst =
    let pairs = List.zip valuelist coordlist
    let neighbors x y a =
        let z = List.map (fun pair -> Map.tryFind pair a) [(x + 1, y); (x + 1, y + 1); (x, y + 1); (x - 1, y + 1); (x - 1, y); (x - 1, y - 1); (x, y - 1); (x + 1, y - 1)]
        List.fold (fun inacc y ->
                                match y with
                                | None -> inacc
                                | Some a -> inacc + a
                              ) 0 z
    match axis with
        | X ->
            List.fold (fun x (value, coord) ->
                        //printfn "%i, (%i, %i)" value coord axisconst
                        Map.add (coord, axisconst) (neighbors coord axisconst x) x
                        ) acc pairs
        | Y ->
            List.fold (fun x (value, coord) ->
                        //printfn "%i, (%i, %i)" value coord axisconst
                        Map.add (axisconst, coord) (neighbors axisconst coord x) x
                        ) acc pairs

let daythreeval2 input =
    let (init:Map<int,(int * int)>) = Map.empty.Add(1, (0, 0))
    let relvals = Map.empty.Add((0, 0), 1)
    let rec daythreehelper acc (direction:Direction) lastindex lastlength rel =
        let ((a, b), check) = List.head (Map.toList rel |> List.sortBy (fun (_, i) -> i) |> List.rev)
        if check > input then
            printfn "%A" (Map.toList rel |> List.sortBy (fun (_, i) -> i))
            ((a, b), check)
        else
            if Map.exists (fun k _ -> k = input) acc then
                ((1, 1), 10)
            else
                let (lastx, lasty) = Map.find lastindex acc
                match direction with
                    | Start ->
                        let newAcc = addValues acc X [(lastindex + 1)] [(lastx + 1)] lasty
                        let newRel = addValues2 rel X [(lastindex + 1)] [(lastx + 1)] lasty
                        daythreehelper newAcc North (lastindex + 1) lastlength newRel
                    | North ->
                        let newlen = lastlength + 1
                        let newAcc = addValues acc Y [(lastindex + 1)..(lastindex + newlen)] [(lasty + 1)..(lasty + newlen)] lastx
                        let newRel = addValues2 rel Y [(lastindex + 1)..(lastindex + newlen)] [(lasty + 1)..(lasty + newlen)] lastx
                        daythreehelper newAcc West (lastindex + newlen) newlen newRel
                    | West ->
                        let newlen = lastlength + 1
                        let newAcc = addValues acc X [(lastindex + 1)..(lastindex + newlen)] (List.rev [(lastx - newlen)..(lastx - 1)]) lasty
                        let newRel = addValues2 rel X [(lastindex + 1)..(lastindex + newlen)] (List.rev [(lastx - newlen)..(lastx - 1)]) lasty
                        daythreehelper newAcc South (lastindex + newlen) newlen newRel
                    | South ->
                        let newAcc = addValues acc Y [(lastindex + 1)..(lastindex + lastlength)] (List.rev [(lasty - lastlength)..(lasty - 1)]) lastx
                        let newRel = addValues2 rel Y [(lastindex + 1)..(lastindex + lastlength)] (List.rev [(lasty - lastlength)..(lasty - 1)]) lastx
                        daythreehelper newAcc East (lastindex + lastlength) lastlength newRel
                    | East ->
                        let newAcc = addValues acc X [(lastindex + 1)..(lastindex + lastlength)] [(lastx + 1)..(lastx + lastlength)] lasty
                        let newRel = addValues2 rel X [(lastindex + 1)..(lastindex + lastlength)] [(lastx + 1)..(lastx + lastlength)] lasty
                        daythreehelper newAcc Start (lastindex + lastlength) lastlength newRel
    daythreehelper init Start 1 0 relvals

let daythreeval input =
    let (init:Map<int,(int * int)>) = Map.empty.Add(1, (0, 0))
    let rec daythreehelper acc (direction:Direction) lastindex lastlength =
        if Map.exists (fun k _ -> k = input) acc then
            Map.find input acc
        else
            let (lastx, lasty) = Map.find lastindex acc
            match direction with
                | Start ->
                    let newAcc = addValues acc X [(lastindex + 1)] [(lastx + 1)] lasty
                    daythreehelper newAcc North (lastindex + 1) lastlength
                | North ->
                    let newlen = lastlength + 1
                    let newAcc = addValues acc Y [(lastindex + 1)..(lastindex + newlen)] [(lasty + 1)..(lasty + newlen)] lastx
                    daythreehelper newAcc West (lastindex + newlen) newlen
                | West ->
                    let newlen = lastlength + 1
                    let newAcc = addValues acc X [(lastindex + 1)..(lastindex + newlen)] (List.rev [(lastx - newlen)..(lastx - 1)]) lasty
                    daythreehelper newAcc South (lastindex + newlen) newlen
                | South ->
                    let newAcc = addValues acc Y [(lastindex + 1)..(lastindex + lastlength)] (List.rev [(lasty - lastlength)..(lasty - 1)]) lastx
                    daythreehelper newAcc East (lastindex + lastlength) lastlength
                | East ->
                    let newAcc = addValues acc X [(lastindex + 1)..(lastindex + lastlength)] [(lastx + 1)..(lastx + lastlength)] lasty
                    daythreehelper newAcc Start (lastindex + lastlength) lastlength
    let coord = daythreehelper init Start 1 0
    printfn "found %O for input %i" coord input
    diffLookup coord

let daythree () =
    let test1 = 1
    let test2 = 12
    let test3 = 23
    let test4 = 1024
    printfn "day 3 tests"
    printfn "input one passing: %b" (0 = (daythreeval test1))
    printfn "input two passing: %b" (3 = (daythreeval test2))
    printfn "input three passing: %b" (2 = (daythreeval test3))
    //printfn "input four passing: %b" (31 = (daythreeval test4))
    //printfn "part one output: %i" (daythreeval 277678)
    printfn "part two output: %O" (daythreeval2 277678)
