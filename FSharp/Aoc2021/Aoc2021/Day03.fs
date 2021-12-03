module Aoc2021.Day03

open System
open Xunit
open Utils

let charToInt (c: char) = int (c) - 48

let binArrayToString s =
  s
  |> Seq.fold (fun (c: string) x -> c + x.ToString()) ""

let binaryCount (input: list<string>) =
  let nrOfBits = input.Head.Length
  let pos = [ for i in 0 .. nrOfBits - 1 -> i ]

  let counter i =
    input
    |> List.fold (fun (c: int) s -> charToInt s.[i] + c) 0

  let counts = pos |> List.map counter


  // sneaky string convert, yeah yeah bitshifts would work and are faster, who cares...
  let gamma =
    counts
    |> List.map (fun x -> if x > input.Length / 2 then 1 else 0)

  let epsilon =
    counts
    |> List.map (fun x -> if x < input.Length / 2 then 1 else 0)

  (gamma, epsilon)

let powerConsumption (input: list<string>) =
  let gamma, epsilon = binaryCount input

  let gamma = binArrayToString gamma
  let epsilon = binArrayToString epsilon

  Convert.ToInt32(gamma, 2)
  * Convert.ToInt32(epsilon, 2)

let test =
  let input =
    [ "00100"
      "11110"
      "10110"
      "10111"
      "10101"
      "01111"
      "00111"
      "11100"
      "10000"
      "11001"
      "00010"
      "01010" ]

  let r = powerConsumption input

  Assert.Equal(r, 198)


let day03 =

  test

  let input =
    readLines "input/day03.txt" |> Seq.toList

  let a = powerConsumption input
  (a, 0)
