module DaySeven
open System.IO

let clean (elem:string) =
    elem.Split([|' '|]) |> Array.head;;

let pairs (inp:string list) =
    inp
    |> List.map (fun item ->
                    item.Split([| "->" |], System.StringSplitOptions.None)
                    )
    |> List.map Array.toList

let topkeys (input:string list) =
    input
    |> pairs
    |> List.fold (fun acc value ->
                        let key = List.head value
                                   |> clean
                        Map.add key None acc
                     ) Map.empty

let cleanList (input:string) =
    input.Split([|','|])
    |> Array.map (fun i -> i.Trim())
    |> Array.toList

let addParents (parent:string) (children:string list) (store:Map<string, string option>) =
    List.fold (fun x child ->
                    Map.add child (Some parent) x
                ) store children

let parents (input:string list) (acc:Map<string, string option>) =
    input
    |> pairs
    |> List.fold (fun acc2 value ->
                        match value with
                        | h :: t ->
                            if t = [] then
                                acc2
                            else
                                let parent = clean h
                                let childs = List.head t
                                addParents parent (cleanList childs) acc2
                        | _ -> acc2
                    ) acc

let bottomitem (input:Map<string, string option>) =
    let (value, _) =
        Map.toList input
        |> List.filter (fun (k, v) -> v = None)
        |> List.head
    value

let bottom (input:string list) =
    topkeys input
    |> parents input
    |> bottomitem

type Node = {
    Weight: int;
    Children: Option<string list>;
    Name: string
}

let parsechildren (children:string list) =
    match children with
    | a :: _ ->
        Some (cleanList a)
    | [] -> None

let extract (elem:string) =
    let k :: w :: _ = elem.Split([|' '|])
                        |> Array.toList
    let weight = String.filter (fun i -> (i = '(' || i = ')') |> not) w
                 |> int
    (k, weight)

let alleven (values:int list) =
    let init = List.head values
    List.fold (fun _ v -> init = v) true values

let rec nodeweight (start:string) (tree:Map<string, Node>) =
    let node = Map.find start tree
    match node.Children with
    | None -> node.Weight
    | Some children ->
        let childrenweight = List.map (fun child -> nodeweight child tree) children
        if alleven childrenweight |> not then
            let culprits = List.zip children childrenweight
            printfn "%O" culprits
            // from here I just did a little bit of hand math!
        node.Weight + (List.sum childrenweight)

let weightchange (input: string list) =
    let kvpairs = input
                |> pairs
    let tree = List.fold (fun acc value ->
                        let program :: rawchildren = value
                        let (key, weight) = extract program
                        let children = parsechildren rawchildren
                        Map.add key { Weight = weight; Name = key; Children = children }  acc
                     ) Map.empty kvpairs
    nodeweight "xegshds" tree

let dayseven() =
    let testin = File.ReadLines("day7test") |> Seq.toList
    let actual = File.ReadLines("day7input") |> Seq.toList
    printfn "day seven tests"
    printfn "test input passing: %b" ("tknk" = (bottom testin))
    printfn "%O" (weightchange actual)
    //printfn "test input two passing: %b" (40 = (weightchange testin))
    //printfn "part one output: %s" (bottom actual)
