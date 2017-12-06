module DaySix
open System.IO

let getMax input =
    Array.fold (fun (trackindex, index, max) value ->
        if value > max then
            (trackindex + 1, trackindex, value)
        else
            (trackindex + 1, index, max)
        ) (0, 0, 0) input

let arrToString arr =
    arr
    |> Array.map (fun i -> i.ToString())
    |> String.concat ","

let rec redistribute startkey todist arr =
   let setkey = ((startkey + 1) % Array.length arr)
   let newvalue = arr.[setkey] + 1
   let nextvalue = todist - 1
   Array.set arr setkey newvalue
   if nextvalue < 1 then
        (arrToString arr, Array.copy arr)
   else
        redistribute setkey nextvalue (Array.copy arr)

let daysixsteps input =
    let rec helper lastString visited cycles arr =
        //printfn "%O" visited
        if Set.contains lastString visited then
            cycles
        else
            let (_, startindex, max) = getMax arr
            Array.set arr startindex 0
            let (nextString, newArr) = redistribute startindex max arr
            let newSet = Set.add lastString visited
            helper nextString newSet (cycles + 1) newArr
    helper "" Set.empty 0 input

let daysix () =
    let testin = [|0; 2; 7; 0;|]
    let actual = File.ReadAllLines("day6in") |> Array.map int
    printfn "%A" actual
    printfn "day 6 tests"
    printfn "input one passing: %b" (5 = (daysixsteps testin))
    printfn "input value: %i" (daysixsteps actual)
