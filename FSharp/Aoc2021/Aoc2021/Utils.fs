module Aoc2021.Utils

open System
open System.IO
open System.Text.RegularExpressions
open FSharpx


let charToInt (c: char) = int c - 48

let readLines (filePath: string) =
  File.ReadLines(filePath) |> Seq.cast<string>

//https://stackoverflow.com/questions/6736464/split-seq-in-f
let splitBy f input =
  let i = ref 0

  input
  |> Seq.groupBy
       (fun x ->
         if f x then incr i
         !i)
  |> Seq.map snd

let (|Regex|_|) pattern input =
  let m = Regex.Match(input, pattern)

  if m.Success then
    Some(List.tail [ for g in m.Groups -> g.Value ])
  else
    None

// Very sexy active pattern
// https://stackoverflow.com/questions/3722591/pattern-matching-on-the-beginning-of-a-string-in-f
let (|Prefix|_|) (p: string) (s: string) =
  if s.StartsWith(p) then
    Some(s.Substring(p.Length))
  else
    None



let mapBoolsToNumbers bools =
  bools
  |> Seq.map (fun x -> if x = true then 1 else 0)


let split (sep: string) (str: string) =
  String.splitString [| sep |] StringSplitOptions.None str
