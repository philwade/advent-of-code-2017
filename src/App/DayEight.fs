module DayEight
open System.IO

let getRegisterValue key m =
    match Map.tryFind key m with
    | Some v -> v
    | None -> 0

let checkOperation target operation value (acc:Map<string, int>) =
    let registerValue = getRegisterValue target acc
    let checkValue = int value
    match operation with
    | "==" -> registerValue = checkValue
    | ">" -> registerValue > checkValue
    | "<" -> registerValue < checkValue
    | ">=" -> registerValue >= checkValue
    | "!=" -> registerValue = checkValue |> not
    | "<=" -> registerValue <= checkValue
    | _ -> false

let doOperation target operation value ((acc:Map<string, int>), (max:int)) =
    let registerValue = getRegisterValue target acc
    let changeValue = int value
    match operation with
    | "inc" ->
        let v = registerValue + changeValue
        let m = if v > max then v else max
        (Map.add target v acc, m)
    | "dec" ->
        let v = registerValue - changeValue
        let m = if v > max then v else max
        (Map.add target v acc, m)
    | _ -> (acc, max)

let applyInstruction ((acc:Map<string, int>), max:int) (instruction:string list) =
    let [target; operation; value; _; checkregister; checkoperation; checkvalue] = instruction

    if checkOperation checkregister checkoperation checkvalue acc then
        doOperation target operation value (acc, max)
    else
        (acc, max)

let largest (input:string list) =
    let registers = Map.empty
    let max = 0
    let instructions = List.map (fun (i:string) -> i.Split([|' '|]) |> Array.toList) input
    let (finalstate, finalmax) = List.fold applyInstruction (registers, max) instructions
    let smallest = Map.fold (fun a _ v -> if v < a then v else a) 0 finalstate
    (Map.fold (fun a _ v -> if v > a then v else a) smallest finalstate, finalmax)

let dayeight() =
    let testinput = File.ReadLines("day8test") |> Seq.toList
    let actualinput = File.ReadLines("day8input") |> Seq.toList
    printfn "day eight tests"
    printfn "test one, two passing: %b" ((1, 10) = (largest testinput))
    printfn "output: %O" (largest actualinput)
