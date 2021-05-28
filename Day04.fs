module Day04

let hasAtLeastTwoAdjacentDigits number =
    let numberAsChars = Array.toList <| number.ToString().ToCharArray()
    let tail = List.tail numberAsChars
    let tailess = List.truncate (numberAsChars.Length - 1) numberAsChars
    List.zip tail tailess
        |> List.exists(fun(zipped) -> 
            fst zipped = snd zipped
        )

let listRepeatedNumbers number =
    let numberAsChars = Array.toList <| number.ToString().ToCharArray()
    numberAsChars 
    |> List.fold (fun (accum: char list list) (x: char) ->
        match accum with
        | head :: tail ->
            if (List.head head) = x then (x :: head) :: tail
            else [x] :: (head :: tail)
        | [] -> [[x]]
    ) []
        
let hasOnlyPairOfDigits number =
    listRepeatedNumbers number |> List.exists (fun (chunk) -> chunk.Length = 2)

let onlyIncreasingDigits number = 
    let numberAsString = Array.toList <| number.ToString().ToCharArray()
    numberAsString = List.sort numberAsString

let processInput number1 number2 (rules: (int->bool) list) =
    seq {
        for number in [number1 .. number2] do
            rules |> List.fold (fun (valid: bool) (rule: int->bool) ->
                rule(number) && valid
            ) true 
    } |> Seq.filter(fun(result) -> result)
    |> Seq.length
