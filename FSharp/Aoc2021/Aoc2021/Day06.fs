module Aoc2021.Day06

open System
open System.IO
open Xunit

let countIn toCount input =
    let rec counter input toCount c =
        match input with
        | [] -> c
        | head :: tail ->
            let v = c + (if head = toCount then 1L else 0L)
            counter tail toCount v

    counter input toCount 0L

let mergePairs (input: ('a * 'b) list) mergeFn =
    let rec getValue key lst =
        match lst with
        | [] -> None
        | (k, v) :: tail ->
            if key = k then
                Some(v)
            else
                getValue key tail

    let rec filterValue key lst =
        match lst with
        | [] -> []
        | (k, _) :: tail when key = k -> filterValue key tail
        | head :: tail -> [ head ] @ filterValue key tail

    let rec merge input =
        match input with
        | [] -> []
        | (hKey, hValue) :: tail ->
            match getValue hKey tail with
            | Some v ->
                [ (hKey, (mergeFn hValue v)) ]
                @ merge (filterValue hKey tail)
            | None -> [ (hKey, hValue) ] @ merge tail

    merge input


let simulate (input: 'a list) nrOfDays =
    let buildPairs (input: 'a list) =
        let mapBase =
            [ (0, 0L)
              (1, 0L)
              (2, 0L)
              (3, 0L)
              (4, 0L)
              (5, 0L)
              (6, 0L)
              (7, 0L)
              (8, 0L) ]

        mapBase
        |> List.map (fun (k, v) -> (k, v + (input |> countIn k)))

    let rec doFishStep fishPairs =
        let step pair =
            match pair with
            | day, fishPop when day = 0 -> [ (8, fishPop); (6, fishPop) ]
            | day, fishPop -> [ (day - 1, fishPop) ]

        match fishPairs with
        | [] -> []
        | head :: tail -> step head @ doFishStep tail

    let mutable fishPairs = buildPairs input

    for i = 0 to (nrOfDays - 1) do
        fishPairs <- doFishStep fishPairs
        fishPairs <- mergePairs fishPairs (fun x y -> x + y)

    fishPairs |> List.sumBy snd

let test =
    let input =
        "3,4,3,1,2"
            .Split(",", StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map int
        |> Seq.toList

    let a = simulate input 80
    let b = simulate input 256

    Assert.Equal(a, 5934L)
    Assert.Equal(b, 26984457539L)


let day06 =
    test

    let input = File.ReadAllText "input/day06.txt"

    let fishes =
        input.Split(",", StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map int
        |> Seq.toList

    let a = simulate fishes 80
    let b = simulate fishes 256
    (a, b)
