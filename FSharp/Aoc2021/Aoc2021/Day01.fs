module Aoc2021.Day01

open Xunit
open Utils

let countIncrease (inputList: list<int>) =
    let rec counter input i =
        match input with
        | head :: second :: tail ->
            if head < second then
                counter (second :: tail) i + 1
            else
                counter (second :: tail) i
        | [ _ ] -> i
        | [] -> i

    counter inputList 0

let slidingWindowIncrease (inputList: list<int>) =
    let comparer first second =
        if Seq.sum (first) < Seq.sum (second) then
            1
        else
            0

    let rec counter input i =
        match input with
        // Niet de mooiste manier, maar werkt als een trein
        | p1 :: p2 :: p3 :: p4 :: tail ->
            let newI =
                i + comparer [ p1; p2; p3 ] [ p2; p3; p4 ]

            counter (p2 :: p3 :: p4 :: tail) newI

        | _ -> i

    counter inputList 0

// TESTS
let seven =
    let data =
        [ "199"
          "200"
          "208"
          "210"
          "200"
          "207"
          "240"
          "269"
          "260"
          "263" ]

    let asInts = data |> Seq.toList |> List.map int
    let result = countIncrease asInts
    Assert.Equal(7, result)

let slidingFive =
    let data =
        [ "199"
          "200"
          "208"
          "210"
          "200"
          "207"
          "240"
          "269"
          "260"
          "263" ]

    let asInts = data |> Seq.toList |> List.map int
    let result = slidingWindowIncrease asInts
    Assert.Equal(5, result)


let day01 =

    // Tests
    seven
    slidingFive

    let input =
        readLines "input/day01.txt"
        |> Seq.toList
        |> List.map int

    let a = countIncrease input
    let b = slidingWindowIncrease input
    (a, b)
