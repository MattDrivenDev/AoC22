open System
open System.IO

type Draw = | Rock | Paper | Scissors

type Result =  | Win of Draw | Draw of Draw | Lose of Draw

let scoreDraw = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

/// Hooray for the tie-fighter infix! :)
let score = function
    | Win d ->  6 |>(+)<| scoreDraw d
    | Draw d -> 3 |>(+)<| scoreDraw d
    | Lose d -> 0 |>(+)<| scoreDraw d

let parseDraw = function
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | s -> s |> sprintf "Invalid input for draw: %s" |> failwith

let evaluate (oponent, me) = 
    match (oponent, me) with
    | (Rock, Rock) -> Draw me
    | (Rock, Paper) -> Win me
    | (Rock, Scissors) -> Lose me
    | (Paper, Rock) -> Lose me
    | (Paper, Paper) -> Draw me
    | (Paper, Scissors) -> Win me
    | (Scissors, Rock) -> Win me
    | (Scissors, Paper) -> Lose me
    | (Scissors, Scissors) -> Draw me

let parseDraws (oponent, me) = 
    (parseDraw oponent, parseDraw me)

let splitLine (s : string) = 
    let xs = s.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
    xs.[0], xs.[1]

"adventofcode.com_2022_day_2_input.txt"
|> File.ReadAllLines
|> Seq.map splitLine
|> Seq.map parseDraws
|> Seq.map evaluate
|> Seq.map score
|> Seq.sum
|> printfn "%A"