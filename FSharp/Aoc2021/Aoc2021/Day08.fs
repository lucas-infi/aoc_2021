module Aoc2021.Day08

open Xunit
open System.IO
open System


//
//               0:      1:      2:      3:      4:
//           aaaa    ....    aaaa    aaaa    ....
//          b    c  .    c  .    c  .    c  b    c
//          b    c  .    c  .    c  .    c  b    c
//           ....    ....    dddd    dddd    dddd
//          e    f  .    f  e    .  .    f  .    f
//          e    f  .    f  e    .  .    f  .    f
//           gggg    ....    gggg    gggg    ....
//
//            5:      6:      7:      8:      9:
//           aaaa    aaaa    aaaa    aaaa    aaaa
//          b    .  b    .  .    c  b    c  b    c
//          b    .  b    .  .    c  b    c  b    c
//           dddd    dddd    ....    dddd    dddd
//          .    f  e    f  .    f  e    f  .    f
//          .    f  e    f  .    f  e    f  .    f
//           gggg    gggg    ....    gggg    gggg


type RawMessage =
    { Alphabet: string []
      Code: string [] }


let overlap (l: string) (r: string) =
    let ls = Set(l)
    let rs = Set(r)
    ls |> Set.isProperSuperset rs

let substract (l: string) (r: string) =
    let ls = Set(l)
    let rs = Set(r)
    ls - rs |> Set.toArray |> String


let buildSimple (s: string) =
    match s with
    | s when s.Length = 2 -> Some((1, s))
    | s when s.Length = 3 -> Some((7, s))
    | s when s.Length = 4 -> Some((4, s))
    | s when s.Length = 7 -> Some((8, s))
    | _ -> None

let getCodeForNumber (num: int) (codex: (int * string) list) =
    let _, s =
        codex |> List.find (fun (v, s) -> v = num)

    s


let parseComplex (codex: (int * string) list) (code: string) =
    let overlapFor s i =
        let entry = getCodeForNumber i codex
        overlap entry s
        
    let rec notInCodex s codex =
        match codex with
        | [] -> false
        | head :: tail -> if codex |> Seq.contains head then true else notInCodex s tail   

    let checkForFive s =
        let nineSubThree =
            substract (getCodeForNumber 9 codex) (getCodeForNumber 3 codex)
        overlap s nineSubThree

    match code with
    | s when (s.Length = 5 && (overlapFor s 1)) -> Some(5, s)
    | s when (s.Length = 6 && (overlapFor s 4)) -> Some(9, s)
    | s when (s.Length = 5 && (checkForFive s)) -> Some(5, s)
//    | s when (s.Length = 5) && notInCodex s codex -> Some(2, s)
//    | s when (s.Length = 6) -> Some(0, s)
//    | s when (s.Length = 6) -> Some(6, s)
    | _ -> None


let buildMessage (msg: RawMessage) =
    let simplestNumbers input =
        input
        |> Seq.map (fun s -> buildSimple s)
        |> Seq.map
            (fun x ->
                match x with
                | Some (v, s) -> (v, s)
                | None -> (-1, ""))
        |> Seq.filter (fun (v, s) -> v <> -1)

    let knowns =
        simplestNumbers msg.Alphabet |> Seq.toList

    // For entire list run through parseComplex, extent parsecomplex till we have no more None returning
    // then return the codex and use is to read the input code
    let rec doMagic codex (msg: string list) =
        match msg with
        | head :: tail ->
            let r = parseComplex codex head

            match r with
            | Some (v) -> doMagic (v :: codex) tail
            | None -> doMagic codex (tail @ [ head ])
        | [] -> codex

    let a = knowns |> Seq.map (fun (v, s) -> s)

    let unknowns =
        msg.Alphabet
        |> Seq.filter (fun x -> a |> Seq.contains x)

    let codex =
        doMagic knowns (msg.Alphabet |> Seq.toList)

    0


let readNumbers (input: RawMessage list) =
    let knwonNumbers =
        input |> Seq.map buildMessage |> Seq.toList

    [ 0 ]

let countSimpleNumbers (input: string [] list) =


    let checkString input =
        input
        |> Seq.map (fun s -> buildSimple s)
        |> Seq.sumBy
            (fun x ->
                match x with
                | Some _ -> 1
                | None -> 0)

    let a = input |> Seq.map checkString |> Seq.sum
    a

let createInputFromString (input: string) =
    input.Split("\n", StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun s -> s.Split("|") |> Seq.last)
    |> Seq.map (fun s -> s.Split(" ", StringSplitOptions.RemoveEmptyEntries))
    |> Seq.toList

let createInput2FromString (input: string) =
    input.Split("\n", StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun s -> s.Split("|"))
    |> Seq.map
        (fun pair ->
            { Alphabet =
                  pair.[0]
                      .Split(" ", StringSplitOptions.RemoveEmptyEntries)
                  |> Seq.toArray
              Code =
                  pair.[1]
                      .Split(" ", StringSplitOptions.RemoveEmptyEntries)
                  |> Seq.toArray })
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

    let input1 = createInputFromString input
    let a = countSimpleNumbers input1

    let b =
        readNumbers (createInput2FromString input)
        |> Seq.sum

    Assert.Equal(26, a)
    Assert.Equal(61229, b)
    0

let day08 =
    test

    let input = File.ReadAllText "input/day08.txt"
    let input = createInputFromString input
    let a = countSimpleNumbers input

    (a, 0)
