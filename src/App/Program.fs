﻿// Learn more about F# at http://fsharp.org

open System
open System.IO


let dayfivesteps input =
    let rec helper steps index newin =
       if ((Array.length newin) - 1) < index then
            steps
       else
            let newsteps = steps + 1
            let nextstep = newin.[index]
            Array.set newin index (nextstep + 1)
            helper newsteps (index + nextstep) newin
    helper 0 0 input

let dayfivestepsv2 input =
    let rec helper steps index newin =
       if ((Array.length newin) - 1) < index then
            steps
       else
            let newsteps = steps + 1
            let nextstep = newin.[index]
            if nextstep >= 3 then
                Array.set newin index (nextstep - 1)
            else
                Array.set newin index (nextstep + 1)
            helper newsteps (index + nextstep) newin
    helper 0 0 input

let dayfive () =
    let test51 = [| 0; 3; 0; 1; -3;|]
    let actual = File.ReadAllLines("day5input") |> Array.map int
    printfn "day 5 tests"
    printfn "input one passing: %b" (5 = (dayfivesteps test51))
    printfn "input one v2 passing: %b" (10 = (dayfivestepsv2 test51))
    printfn "part one value: %i" (dayfivestepsv2 actual)

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

//let addValues acc index axis (valstart, valend) (coordstart, coordend) =
    //let pairs = List.zip [valstart..valend] [coordstart..coordend]
    //List.fold (fun (value, coord) acc2 = 
                //match axis with
                //| X ->
                    //acc2.add value 

let daythreeval input =
    let (init:Map<int,(int * int)>) = Map.empty.Add(1, (0, 0))
    let rec daythreehelper acc (direction:Direction) length index =
        if Map.exists (fun k _ -> k = input) acc then
            Map.find input acc
        else
           // match direction with
            //| Start ->
             //   let (newAcc, newIndex) = addValues acc
              //  daythreehelper newAcc North length newIndex
            (0, 0)
    let coord = daythreehelper init Start 2 2
    diffLookup coord

let daythree () =
    let test1 = 1
    let test2 = 12
    let test3 = 23
    let test4 = 1024
    printfn "day 3 tests"
    printfn "input one passing: %b" (0 = (daythreeval test1))
    printfn "input two passing: %b" (12 = (daythreeval test2))
    printfn "input three passing: %b" (23 = (daythreeval test3))
    printfn "input four passing: %b" (1024 = (daythreeval test4))

let rec dayonesum numbers =
    let origHead = List.head numbers
    let rec sum innumbers =
        match innumbers with
        | head :: (head2 :: tail) ->
            if head = head2 then
                head + (sum (head2 :: tail))
            else
                sum (head2 :: tail)
        | [one] ->
            if one = origHead then
                one
            else
                0
        | _ -> 0
    sum numbers

let circularsum numbers =
    let length = Array.length numbers
    let half = length / 2
    Array.indexed numbers
               |> Array.filter (fun (i, elem) -> numbers.[((i + half) % length)] = elem)
               |> Array.map (fun (i, elem) -> elem)
               |> Array.sum


let dayone () =
    let input1 = "61697637962276641366442297247367117738114719863473648131982449728688116728695866572989524473392982963976411147683588415878214189996163533584547175794158118148724298832798898333399786561459152644144669959887341481968319172987357989785791366732849932788343772112176614723858474959919713855398876956427631354172668133549845585632211935573662181331613137869866693259374322169811683635325321597242889358147123358117774914653787371368574784376721652181792371635288376729784967526824915192526744935187989571347746222113625577963476141923187534658445615596987614385911513939292257263723518774888174635963254624769684533531443745729344341973746469326838186248448483587477563285867499956446218775232374383433921835993136463383628861115573142854358943291148766299653633195582135934544964657663198387794442443531964615169655243652696782443394639169687847463721585527947839992182415393199964893658322757634675274422993237955354185194868638454891442893935694454324235968155913963282642649968153284626154111478389914316765783434365458352785868895582488312334931317935669453447478936938533669921165437373741448378477391812779971528975478298688754939216421429251727555596481943322266289527996672856387648674166997731342558986575258793261986817177487197512282162964167151259485744835854547513341322647732662443512251886771887651614177679229984271191292374755915457372775856178539965131319568278252326242615151412772254257847413799811417287481321745372879513766235745347872632946776538173667371228977212143996391617974367923439923774388523845589769341351167311398787797583543434725374343611724379399566197432154146881344528319826434554239373666962546271299717743591225567564655511353255197516515213963862383762258959957474789718564758843367325794589886852413314713698911855183778978722558742329429867239261464773646389484318446574375323674136638452173815176732385468675215264736786242866295648997365412637499692817747937982628518926381939279935993712418938567488289246779458432179335139731952167527521377546376518126276"
    //let intinput1 = [for c in input1 -> string c |> int]
    printfn "question 1 tests"
    let test1 = [1; 1; 2; 2;]
    let test2 = [1; 1; 1; 1;]
    let test3 = [1; 2; 3; 4;]
    let test4 = [9; 1; 2; 1; 2; 1; 2; 9;]
    printfn "input one passing: %b" (3 = (dayonesum test1))
    printfn "input two passing: %b" (4 = (dayonesum test2))
    printfn "input three passing: %b" (0 = (dayonesum test3))
    printfn "input four passing: %b" (9 = (dayonesum test4))
    //printfn "first answer for input: %i" (dayonesum intinput1)
    let intinput2 = [|for c in input1 -> string c |> int|]
    let test21 = [|1; 2; 1; 2;|]
    let test22 = [|1; 2; 2; 1;|]
    let test23 = [|1; 2; 3; 4; 2; 5;|]
    let test24 = [|1; 2; 3; 1; 2; 3;|]
    let test25 = [|1; 2; 1; 3; 1; 4; 1; 5;|]
    printfn "input 2-one passing: %b" (6 = (circularsum test21))
    printfn "input 2-two passing: %b" (0 = (circularsum test22))
    printfn "input 2-three passing: %b" (4 = (circularsum test23))
    printfn "input 2-four passing: %b" (12 = (circularsum test24))
    printfn "input 2-five passing: %b" (4 = (circularsum test25))
    printfn "second answer for input: %i" (circularsum intinput2)

[<EntryPoint>]
let main argv =
    dayfive()
    0 // return an integer exit code
