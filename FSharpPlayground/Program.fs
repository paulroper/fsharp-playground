open FSharpPlayground

[<EntryPoint>]
let main argv =
    let input = [2;5;7;9;8;1;3;6;4]

    printfn "Sorting list %A" input

    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    let result = Sorters.bubbleSortier input
    stopwatch.Stop()

    printfn "Finished sorting in %dms. Result of the sort is %A" stopwatch.ElapsedMilliseconds result

    0 // return an integer exit code
