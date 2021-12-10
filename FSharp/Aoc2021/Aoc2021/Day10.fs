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

let isClosingFor opening ch = (int ch) - (int opening) <= 2

let push (c: 'a) (lst: 'a list) = lst @ [ c ]

let pop (lst: 'a list) = lst |> List.take (lst.Length - 2)

let peek (lst: 'a list) = (lst |> List.last)

let findBadChar (line: string) =
    let asChars = line.ToCharArray() |> Seq.toList
    let mutable stack = []

    let doStuff (line: string) =
        for c in line do
            let r =
                match c with
                | c when isOpening c ->
                    stack <- push c stack
                    None
                | c when isClosingFor (peek stack) c ->
                    stack <- pop stack
                    None
                | c when isClosing c -> Some(c)
            

    '0'

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
        lines |> Seq.map findBadChar |> Seq.toList
    //        |> Seq.map (fun x -> getScore x)
//        |> Seq.sum

    score
    0


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

    Assert.Equal('0', findBadChar "()")
    Assert.Equal('0', findBadChar "{}")
    Assert.Equal('0', findBadChar "[]")
    Assert.Equal('0', findBadChar "([])")
    Assert.Equal('0', findBadChar "([][][])")
    Assert.Equal('0', findBadChar "(())")
    Assert.Equal('0', findBadChar "[<>({}){}[([])<>]]")
    Assert.Equal('0', findBadChar "(((((((((())))))))))")
    Assert.Equal('0', findBadChar "(((")
    Assert.Equal('}', findBadChar "(}")
    Assert.Equal('}', findBadChar "[(}]")
    Assert.Equal('>', findBadChar "{()()()>")
    Assert.Equal('}', findBadChar "(((()))}")
    Assert.Equal(')', findBadChar "<([]){()}[{}])")
    Assert.Equal(')', findBadChar "((<([]){()}[{}])))")


    let input =
        input.Split("\n", StringSplitOptions.RemoveEmptyEntries)

    let a = calcFirstOutput input
    Assert.Equal(26397, a)

    0

let day10 =
    test |> ignore

    (0, 0)
