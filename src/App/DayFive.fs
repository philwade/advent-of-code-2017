module DayFive
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
