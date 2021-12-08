module Aoc2021.Day08


open Xunit
open System.IO
open System

    
let countSimpleNumbers (input: string [] list) =
    let isSimple (s: string) =
        match s with
        | s when s.Length = 2 -> Some((1, s))
        | s when s.Length = 3 -> Some((7, s))
        | s when s.Length = 4 -> Some((4, s))
        | s when s.Length = 7 -> Some((8, s))
        | _ -> None

    let checkString input =
        input
        |> Seq.map (fun s -> isSimple s)
        |> Seq.sumBy
            (fun x ->
                match x with
                | Some (_) -> 1
                | None -> 0)

    let a = input |> Seq.map checkString |> Seq.sum
    a

let createInputFromString (input: string) =
    input.Split("\n", StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun s -> s.Split("|") |> Seq.last)
    |> Seq.map (fun s -> s.Split(" ", StringSplitOptions.RemoveEmptyEntries))
    |> Seq.toList

let test =
    let input =
        "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

    let input = createInputFromString input
    let a = countSimpleNumbers input

    Assert.Equal(26, a)
    0

let day08 =
    test

    let input = File.ReadAllText "input/day08.txt"
    let input = createInputFromString input
    let a = countSimpleNumbers input

    (a, 0)
