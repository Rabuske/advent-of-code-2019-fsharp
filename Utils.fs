module Utils

let readLines (dayNumber:string) = 
    $"Inputs\\Day{dayNumber}.txt"
    |> System.IO.File.ReadLines
