module Aoc2021.Day03

open System
open Xunit
open Utils

let charToInt (c: char) = int (c) - 48

let binArrayToString s =
  s
  |> Seq.fold (fun (c: string) x -> c + x.ToString()) ""

let halvedUp (i: int) =
  (Convert.ToInt32(Math.Ceiling((float i) / 2.0)))

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

let oxygenLogic counter (inputList: list<string>) (pos: int) =
  match counter inputList pos with
  | x when x = halvedUp inputList.Length -> 1
  | x when x > inputList.Length / 2 -> 1
  | _ -> 0

let co2Logic counter (inputList: list<string>) (pos: int) =
  match counter inputList pos with
  | x when x = halvedUp inputList.Length -> 0
  | x when x > inputList.Length / 2 -> 0
  | _ -> 1

let postitionCounter (input: list<string>) filterLogic =
  let nrOfBits = input.Head.Length
  let pos = [ for i in 0 .. nrOfBits - 1 -> i ]

  let counter (input: list<string>) (i: int) =
    input
    |> List.fold (fun (c: int) s -> charToInt s.[i] + c) 0

  let rec filter (inputList: list<string>) pos =

    let filterOn = filterLogic counter inputList pos

    let filterred =
      inputList
      |> List.filter (fun x -> (charToInt x.[pos]) = filterOn)

    if filterred.Length = 1 then
      filterred.[0]
    else
      filter filterred (pos + 1)

  Convert.ToInt32(filter input 0, 2)

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

  let oxy = postitionCounter input oxygenLogic
  Assert.Equal(oxy, 23)

  let co2 = postitionCounter input co2Logic
  Assert.Equal(co2, 10)

let day03 =

  test

  let input =
    readLines "input/day03.txt" |> Seq.toList

  let a = powerConsumption input
  let oxy = postitionCounter input oxygenLogic
  let co2 = postitionCounter input co2Logic

  (a, oxy * co2)
