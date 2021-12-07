module Aoc2021.Day07

open Xunit
open System.IO
open System

let gaussSolutionOfSumOverN n =
    if (n % 2) = 0 then
        (n + 1) * (n / 2)
    else
        (n * ((n - 1) / 2)) + n

let moveCost (input: int seq) pos =
    input
    |> Seq.fold (fun acc v -> (acc + Math.Abs(pos - v))) 0

let moveCostLinear (input: int seq) pos =
    input
    |> Seq.fold (fun acc v -> (acc + gaussSolutionOfSumOverN (Math.Abs(pos - v)))) 0

let calcCost (input: int list) costFn =
    let max = input |> Seq.max
    
    let positions = [0 .. max]
    
    let moveCosts =
        positions |> Seq.map (fun x -> (x, costFn input x))

    let _, v =
        moveCosts |> Seq.minBy (fun (_, v) -> v)

    v

let test =
    let input =
        "16,1,2,0,4,2,7,1,2,14".Split(",")
        |> Seq.map int
        |> Seq.toList

    let a = calcCost input moveCost
    let b = calcCost input moveCostLinear

    Assert.Equal(37, a)
    Assert.Equal(168, b)

let day07 =
    test

    let input = File.ReadAllText "input/day07.txt"

    let input =
        input.Split(",", StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map int
        |> Seq.toList

    let a = calcCost input moveCost
    let b = calcCost input moveCostLinear

    (a, b)
