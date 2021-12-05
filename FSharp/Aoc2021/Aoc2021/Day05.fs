module Aoc2021.Day05

open System
open Xunit
open Utils

type Point = { x: int; y: int }

let calculateVertAndHortOverlap (input: string list) =

  let pairs =
    input
    |> Seq.map (fun s -> s.Split(" -> ", StringSplitOptions.TrimEntries))
    |> Seq.map
         (fun s ->
           let one = s.[0].Split(',') |> Array.map int
           let two = s.[1].Split(',') |> Array.map int
           ({ x = two.[0]; y = two.[1] }, { x = one.[0]; y = one.[1] }))
    |> Seq.toList

  let maxX =
    pairs
    |> List.fold (fun acc (p1, p2) -> [ acc; p1.x; p2.x ] |> List.max) 0

  let maxY =
    pairs
    |> List.fold (fun acc (p1, p2) -> [ acc; p1.y; p2.y ] |> List.max) 0


  let plotAndCount pairs =
    let field = Array2D.zeroCreate (maxX + 1) (maxY + 1)

    for p1, p2 in pairs do
      let bigX = if p1.x > p2.x then p1.x else p2.x
      let bigY = if p1.y > p2.y then p1.y else p2.y
      let smallX = if p1.x <= p2.x then p1.x else p2.x
      let smallY = if p1.y <= p2.y then p1.y else p2.y

      for i = smallX to bigX do
        for j = smallY to bigY do
          let v = field.[i, j]
          Array2D.set field i j (v + 1)

    for i = 0 to maxX do
      for j = 0 to maxY do
        printf $"{field.[i, j]}"

      printfn ""

    printfn ""

    let mutable c = 0

    field
    |> Array2D.iter
         (fun v ->
           match v with
           | v when v >= 2 -> c <- c + 1
           | _ -> ())

    c


  let HorVPairs =
    pairs
    |> List.filter (fun (p1, p2) -> p1.x = p2.x || p1.y = p2.y)

  let diagVPars =
    pairs
    |> List.filter (fun (p1, p2) -> Math.Abs(p2.x - p1.x) = Math.Abs(p2.y - p1.y))

  let a = plotAndCount HorVPairs
  let b = plotAndCount (diagVPars @ HorVPairs)
  (a, b)


let test =
  let input =
    [ "0,9 -> 5,9"
      "8,0 -> 0,8"
      "9,4 -> 3,4"
      "2,2 -> 2,1"
      "7,0 -> 7,4"
      "6,4 -> 2,0"
      "0,9 -> 2,9"
      "3,4 -> 1,4"
      "0,0 -> 8,8"
      "5,5 -> 8,2" ]

  let a, b = calculateVertAndHortOverlap input

  Assert.Equal(5, a)
  Assert.Equal(12, b)

let day05 =
  test

  let input =
    readLines "input/day05.txt" |> Seq.toList

  let a, b = calculateVertAndHortOverlap input
  (a, b)
