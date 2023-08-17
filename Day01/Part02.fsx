open System
open System.IO

/// Splits the input string into a sequence of groups of lines ready
/// to be parsed into integers.
let split (s : string) = 
    s.Split([|"\n\n"|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun s' -> s'.Split([|"\n"|], StringSplitOptions.RemoveEmptyEntries))

"adventofcode.com_2022_day_1_input.txt"
|> File.ReadAllText
|> split
|> Seq.map (Seq.map int)
|> Seq.map Seq.sum
|> Seq.sortDescending
|> Seq.take 3
|> Seq.sum
|> printfn "%A"