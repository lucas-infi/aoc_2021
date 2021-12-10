module Aoc2021.Day10

open System
open Utils
open Xunit

let isOpening c =
    match c with
    | '(' -> true
    | '[' -> true
    | '{' -> true
    | '<' -> true
    | _ -> false

let isClosing c =
    match c with
    | ')' -> true
    | ']' -> true
    | '}' -> true
    | '>' -> true
    | _ -> false

let findBadChar (line: string) =
    let asChars = line.ToCharArray() |> Seq.toList

    let mutable counter = -1
    
    let rec returnUnBalancedChar br rest =
        counter <- counter + 1

        match rest with
        | [] -> []
        | head :: tail ->
            match head with
            | head when isOpening head -> returnUnBalancedChar head tail
            | head when (((int head) - (int br)) <= 2) -> '0' :: returnUnBalancedChar head tail
            | _ -> [ head ]

    let a =
        returnUnBalancedChar (asChars |> Seq.head) (asChars |> List.skip 1)

    a


let calcFirstOutput (lines: string []) =
    let wheighing =
        [ (')', 3)
          (']', 57)
          ('}', 1197)
          ('>', 25137)
          ('0', 0) ]

    let getScore c =
        let _, v =
            wheighing |> Seq.find (fun (cc, _) -> cc = c)

        v

    let score =
        lines
        |> Seq.map findBadChar
        |> Seq.filter (fun ca -> ca |> Seq.exists (fun c -> isClosing c))
        |> Seq.toList
    //        |> Seq.map (fun x -> getScore x)
//        |> Seq.sum

    score


let test =
    let input =
        "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"

    let input =
        input.Split("\n", StringSplitOptions.RemoveEmptyEntries)

    let a = calcFirstOutput input
    //    Assert.Equal(26397, a)

    0

let day10 =
    test |> ignore

    (0, 0)
