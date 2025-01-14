open Interpolation
open Utils
open Plotly.NET

[<EntryPoint>]
let main (args) =
    let stepArg = Parser.tryParseFloat args[0]

    let methodsArg = args |> Seq.skip 1 |> Seq.map Utils.parseArgumentMethod

    if stepArg.IsSome && methodsArg |> Seq.forall Result.isOk then

        let step = stepArg.Value
        printfn "1. Chosen step: %.2f" step

        let methods = methodsArg |> Seq.choose Result.toOption
        printfn "2. Chosen methods: %A" methods

        let points =
            Utils.readLines
            |> Seq.map Validator.parseAndValidate
            |> Seq.choose (fun parseRes ->
                match parseRes with
                | Ok point -> Some point
                | Error errList ->
                    printfn "Found errors:"

                    errList
                    |> Seq.map Validator.errorToString
                    |> Seq.reduce (fun x y -> x + "\n" + y)
                    |> printfn "%s"

                    printfn "Skipping line"
                    None)
            |> Seq.cache

        interpolateStream points methods step
        |> Seq.iter (fun out_seq ->
            out_seq
            |> Seq.zip methods
            |> Seq.iter (fun (m, maybe_points) ->
                match maybe_points with
                | Some out_seq ->
                    printfn "%A" m
                    out_seq |> Seq.map fst |> Seq.iter (printf "%0.2f\t")
                    printfn ""
                    out_seq |> Seq.map snd |> Seq.iter (printf "%0.2f\t")
                    printfn "\n"
                | None -> ()))

        0
    else
        if stepArg.IsNone then
            printfn "Unexpected step value: %s" args[0]

        methodsArg
        |> Seq.choose (fun r ->
            match r with
            | Ok r -> None
            | Error s -> Some s)
        |> Seq.iter (printfn "%s")

        printfn "Usage: step interpolate [linear|lagrange={int16}]..."
        0