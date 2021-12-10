module Aoc2021.Day09

open System
open System.IO
open Xunit
open Utils


let isLowerThenRest i j (map: int [,]) =
    let c = map.[i, j]

    c < map.[i + 1, j]
    && c < map.[i - 1, j]
    && c < map.[i, j + 1]
    && c < map.[i, j - 1]

let createMap (mapStr: string) =
    let map: int [] [] =
        mapStr.Split("\n", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> s.ToCharArray() |> Array.map charToInt)

    let intMap = array2D map

    let walled =
        Array2D.create (intMap.GetLength(0) + 2) (intMap.GetLength(1) + 2) 9

    Array2D.blit intMap 0 0 walled 1 1 (intMap.GetLength(0)) (intMap.GetLength(1))
    (intMap.GetLength(0), intMap.GetLength(1), walled)

let rec findLowestPoint (mapStr: string) =
    let xSize, ySize, intMap = createMap mapStr

    let mutable postions = []

    for i = 1 to xSize do
        for j = 1 to ySize do
            match isLowerThenRest i j intMap with
            | true -> postions <- ((i - 1, j - 1), intMap.[i, j]) :: postions
            | false -> ()

    postions

let sumOfRisks (points: ((int * int) * int) list) =
    points |> List.sumBy (fun ((_, _), v) -> v + 1)

let lowestNeighbour i j (map: int [,]) =
    let min =
        Seq.min (
            map.[i + 1, j]
            :: map.[i - 1, j]
               :: map.[i, j + 1] :: map.[i, j - 1] :: [ map.[i, j] ]
        )

    i, j, min



let findBasins (xSize: int) (ySize: int) (intMap: int [,]) (positions: ((int * int) * int) list) =
    /// Simple flooding Algorithm:
    /// Mark lowest points with their id (negative int value)
    /// alg:
    /// Start with p = 0 to 8
    /// for all p's in map, check if a neighbour is marked,
    /// if true -> mark that p
    /// else -> check if neighbour is p if so check that neighbour's neighbour

    // Mark al points
    let mutable counter = 0

    for p in positions do
        let (i, j), _ = p
        counter <- counter - 1
        intMap.[i + 1, j + 1] <- counter

    for p = 0 to 8 do
        for i = 1 to xSize do
            for j = 1 to ySize do

                let v = intMap.[i, j]

                if v < 0 || v = 9 then
                    ()
                else
                    let x, y, value = lowestNeighbour i j intMap

                    if value < 0 then intMap.[i, j] <- value
                    else if value = intMap.[i, j] then ()

    let mutable flatList = []

    intMap
    |> Array2D.iter
        (fun v ->
            if v > 0 then
                ()
            else
                flatList <- v :: flatList)

    let counts = flatList |> Seq.countBy id

    counts
    |> Seq.map (fun (_, c) -> c)
    |> Seq.sortDescending
    |> Seq.toList
    |> List.take 3
    |> List.fold (fun acc i -> acc * i) 1

let test =
    let input =
        "2199943210
3987894921
9856789892
8767896789
9899965678"


    let lowestPoints = findLowestPoint input
    let a = sumOfRisks lowestPoints
    Assert.Equal(15, a)

    let xSize, ySize, map = createMap input
    let largest3 = findBasins xSize ySize map lowestPoints

    Assert.Equal(1134, largest3)


let day09 =
    let inputStr = File.ReadAllText "input/day09.txt"
    let lowestPoints = findLowestPoint inputStr
    let a = sumOfRisks lowestPoints

    let xSize, ySize, map = createMap inputStr
    let b = findBasins xSize ySize map lowestPoints


    test |> ignore
    (a, b)
