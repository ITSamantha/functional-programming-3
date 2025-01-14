module Utils

open Interpolation

let (|IsLinear|_|) (s: string) =
    if s.Equals("linear") then Some Linear else None

let (|IsLagrange|_|) (s: string) =
    match s.Split("=") with
    | [| "lagrange"; value |] ->
        match Parser.tryParseInt16 value with
        | Some(int) -> Some(Lagrange(int))
        | None -> None
    | _ -> None

let parseArgumentMethod (s: string) =
    match s with
    | IsLinear m -> Ok m
    | IsLagrange m -> Ok m
    | _ -> Error(sprintf "Unknown arg %s" s)

let readLines =
    Seq.initInfinite (fun _ ->
        try
            System.Console.ReadLine()
        with ex ->
            null)
    |> Seq.takeWhile (not << isNull)
