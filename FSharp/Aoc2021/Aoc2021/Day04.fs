module Aoc2021.Day04

open System
open System.IO
open Xunit

type Score = { Number: int; Value: int }
type BoardValues = { Values: int list; Score: int }

type Board =
  { Rows: list<BoardValues>
    Cols: list<BoardValues> }

let calcRow (row: int list) (calledNumbers: Score list) =
  let findScore (r: int) =
    match calledNumbers
          |> List.tryFind (fun x -> x.Number = r) with
    | Some (s) -> s.Value
    | _ -> Int32.MaxValue

  row
  |> List.fold
       (fun acc x ->
         if (findScore x) > acc then
           (findScore x)
         else
           acc)
       0

let calculateWinner (boards: Board list) (calledNumbers: Score list) =
  let lowestScoreInBoard (board: Board) =
    let minRow =
      board.Rows
      |> Seq.map (fun r -> r.Score)
      |> Seq.min

    let minCol =
      board.Cols
      |> Seq.map (fun r -> r.Score)
      |> Seq.min

    if minRow < minCol then
      minRow
    else
      minCol

  boards
  |> Seq.map (fun b -> (b, lowestScoreInBoard b))
  |> Seq.sortBy (fun (b, sc) -> sc)
  |> Seq.head

let calculateLoser (boards: Board list) (calledNumbers: Score list) =
  let lowestScoreInBoard (board: Board) =
    let minRow =
      board.Rows
      |> Seq.map (fun r -> r.Score)
      |> Seq.min

    let minCol =
      board.Cols
      |> Seq.map (fun r -> r.Score)
      |> Seq.min

    if minRow < minCol then
      minRow
    else
      minCol

  boards
  |> Seq.map (fun b -> (b, lowestScoreInBoard b))
  |> Seq.sortBy (fun (b, sc) -> sc)
  |> Seq.last



let calculateScore (boardAndScore: Board * int) (calledNumbers: Score list) =
  let board, (score: int) = boardAndScore

  let calledUpTo =
    calledNumbers
    |> List.take (score + 1)
    |> List.map (fun m -> m.Number)

  let small =
    board.Rows
    |> Seq.map (fun bv -> bv.Values)
    |> Seq.concat
    |> Seq.filter (fun v -> not (List.contains v calledUpTo))
    |> Seq.sum

  let lastDraw = calledNumbers.[score].Number
  small * lastDraw


let createBoards (inputList: string) (calledNumber: Score list) =
  let boardStrings =
    inputList.Trim().Split("\n\n", StringSplitOptions.RemoveEmptyEntries).[1..]

  let createBoardFromString (str: string) =
    let rows: int list list =
      str.Split("\n", StringSplitOptions.RemoveEmptyEntries)
      |> Seq.map
           (fun s ->
             s.Split(" ", StringSplitOptions.RemoveEmptyEntries)
             |> Seq.map int
             |> Seq.toList)
      |> Seq.toList

    let colBuild (rows: int list list) i =
      rows
      |> List.fold (fun l (x: int list) -> x.[i] :: l) []
      |> List.rev

    // Assuming al boards are 5 by 5
    let cols =
      [ colBuild rows 0
        colBuild rows 1
        colBuild rows 2
        colBuild rows 3
        colBuild rows 4 ]

    { Rows =
        rows
        |> List.map
             (fun l ->
               { Values = l
                 Score = calcRow l calledNumber })
      Cols =
        cols
        |> List.map
             (fun l ->
               { Values = l
                 Score = calcRow l calledNumber }) }

  let boards =
    boardStrings
    |> Seq.map createBoardFromString
    |> Seq.toList

  boards

let firstCardToWin (input: string) =
  let numbers =
    input.Trim().Split("\n").[0]
      .Split(',', StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map int
    |> Seq.mapi (fun i x -> { Number = x; Value = i })
    |> Seq.toList

  let boards = createBoards input numbers
  let winner = calculateWinner boards numbers
  let score = calculateScore winner numbers

  let loser = calculateLoser boards numbers
  let loserScore = calculateScore loser numbers
  (score, loserScore)

let test =
  let input =
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
 "

  let (a, b) = firstCardToWin input


  Assert.Equal(a, 4512)
  Assert.Equal(b, 1924)

let day04 =
  test

  let input = File.ReadAllText "input/day04.txt"
  let a, b = firstCardToWin input

  (a, b)
