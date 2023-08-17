open System
open System.IO

type Draw = | Rock | Paper | Scissors

type Result =  | Win of Draw | Draw of Draw | Lose of Draw

type Strategy = | PlayToWin | PlayToDraw | PlayToLose

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
    | s -> s |> sprintf "Invalid input for draw: %s" |> failwith

let parseStrategy = function
    | "X" -> PlayToLose
    | "Y" -> PlayToDraw
    | "Z" -> PlayToWin
    | s -> s |> sprintf "Invalid input for strategy: %s" |> failwith

let evaluate (oponent, strategy) = 
    match (oponent, strategy) with
    | (Rock, PlayToWin) -> Win Paper
    | (Rock, PlayToDraw) -> Draw Rock
    | (Rock, PlayToLose) -> Lose Scissors
    | (Paper, PlayToWin) -> Win Scissors
    | (Paper, PlayToDraw) -> Draw Paper
    | (Paper, PlayToLose) -> Lose Rock
    | (Scissors, PlayToWin) -> Win Rock
    | (Scissors, PlayToDraw) -> Draw Scissors
    | (Scissors, PlayToLose) -> Lose Paper

let parse (oponent, me) = 
    (parseDraw oponent, parseStrategy me)

let splitLine (s : string) = 
    let xs = s.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
    xs.[0], xs.[1]

"adventofcode.com_2022_day_2_input.txt"
|> File.ReadAllLines
|> Seq.map splitLine
|> Seq.map parse
|> Seq.map evaluate
|> Seq.map score
|> Seq.sum
|> printfn "%A"