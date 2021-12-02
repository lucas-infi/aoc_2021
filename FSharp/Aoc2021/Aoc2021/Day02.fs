module Aoc2021.Day02

open Xunit
open Utils
open System


let matcher (text: string) (pair: int * int) =
    let x, y = pair

    match text with
    | Prefix "forward " rest -> (x + (int rest), y)
    | Prefix "down " rest -> (x, y + (int rest))
    | Prefix "up " rest -> (x, y - (int rest))
    | _ -> (x, y)

let calculatePosition (inputList: list<string>) =
    let rec calculator inputs (pair: int * int) =
        match inputs with
        | head :: tail -> calculator tail (matcher head pair)
        | [] -> pair

    let x, y = calculator inputList (0, 0)
    x * y


let matcherWithAim (text: string) (pair: int * int * int) =
    let (dx: int, da: int) =
        match text with
        | Prefix "forward " rest -> (int rest, 0)
        | Prefix "down " rest -> (0, int rest)
        | Prefix "up " rest -> (0, - int rest)
        | _ -> (0, 0)

    let x, y, a = pair
    (x + dx, y + a * dx, a + da)

let calculatePositionWithAim (inputList: list<string>) =
    let rec calculator inputs (pair: int * int * int) =
        match inputs with
        | head :: tail -> calculator tail (matcherWithAim head pair)
        | [] -> pair

    let x, y, _ = calculator inputList (0, 0, 0)
    x * y


// TESTS
let test =
    let input =
        [ "forward 5"
          "down 5"
          "forward 8"
          "up 3"
          "down 8"
          "forward 2" ]

    let result = calculatePosition input
    Assert.Equal(result, 150)

    let result = calculatePositionWithAim input
    Assert.Equal(result, 900)

let day02 =

    // Tests
    test

    let input =
        readLines "input/day02.txt" |> Seq.toList

    let a = calculatePosition input
    let b = calculatePositionWithAim input
    (a, b)
