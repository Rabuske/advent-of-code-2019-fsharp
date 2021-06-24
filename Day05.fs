module Day05

type ParameterMode = Position = 'P' | Immediate = 'I'

type OperationDescription = {
    operationCode: int
    param1Mode: ParameterMode
    param2Mode: ParameterMode
    param3Mode: ParameterMode
}

type OperationValues = {
    value1: int
    value2: int
    value3: int
}

let getTape (inputLine:string) =
    inputLine.Split(",")
    |> Array.map int

let convertToParameterMode input = 
    match input with
        | '0' -> ParameterMode.Position
        | '1' -> ParameterMode.Immediate
        | _ -> ParameterMode.Position

let generateOperationDescription (number:int) = 
    let numberWithLeadingZeroes = number.ToString("D5").ToCharArray()
    let inline charToInt c = int c - int '0'
    let operation = charToInt(numberWithLeadingZeroes.[3]) + charToInt(numberWithLeadingZeroes.[4])
    {
        operationCode = operation
        param1Mode = convertToParameterMode numberWithLeadingZeroes.[2]
        param2Mode = convertToParameterMode numberWithLeadingZeroes.[1]
        param3Mode = convertToParameterMode numberWithLeadingZeroes.[0]
    }

let extractValuesForOperation (operationDescription:OperationDescription) (tape:int[]) (position:int) (numberOfParameters: int) : OperationValues = 
    let parameter1 = if numberOfParameters >= 1 then tape.[position + 1] else -1
    let parameter2 = if numberOfParameters >= 2 then tape.[position + 2] else -1
    let value3 = if numberOfParameters >= 3 then tape.[position + 3] else -1
    let value1 = 
        match operationDescription.param1Mode with
            | ParameterMode.Position -> tape.[parameter1]
            | _ -> parameter1
    let value2 = 
        match operationDescription.param2Mode with
            | ParameterMode.Position -> tape.[parameter2]
            | _ -> parameter2
    {value1 = value1; value2 = value2; value3 = value3}

let processMathOperation (operationDescription:OperationDescription) (tape:int[]) (position:int) (operation: int -> int -> int) = 
    let values = extractValuesForOperation operationDescription tape position 3
    tape.[values.value3] <- operation values.value1 values.value2
    
let intCode (instructions: int[]) (input: int) = 
    let mutable tape = instructions
    let rec intCodeClosure (position: int) (output: int list) = 
        let operationDescription = generateOperationDescription tape.[position]
        match operationDescription.operationCode with
            | 1 ->
                processMathOperation operationDescription tape position (+)
                intCodeClosure (position + 4) (output)
            | 2 ->
                processMathOperation operationDescription tape position (*)
                intCodeClosure (position + 4) (output)
            | 3 ->
                let parameter1 = tape.[position + 1]
                tape.[parameter1] <- input
                intCodeClosure (position + 2) (output)
            | 4 -> 
                let parameter1 = tape.[position + 1]
                intCodeClosure (position + 2) (tape.[parameter1]::output)
            | 5 ->
                let values = extractValuesForOperation operationDescription tape position 2
                let nextInstruction = if values.value1 = 0 then position + 3 else values.value2 
                intCodeClosure (nextInstruction) (output)
            | 6 ->
                let values = extractValuesForOperation operationDescription tape position 2
                let nextInstruction = if values.value1 = 0 then values.value2 else position + 3
                intCodeClosure (nextInstruction) (output)
            | 7 ->
                let values = extractValuesForOperation operationDescription tape position 3
                let result = if values.value1 < values.value2 then 1 else 0
                tape.[values.value3] <- result
                intCodeClosure (position + 4) (output)
            | 8 ->
                let values = extractValuesForOperation operationDescription tape position 3
                let result = if values.value1 = values.value2 then 1 else 0
                tape.[values.value3] <- result
                intCodeClosure (position + 4) (output)
            | 99 -> output
            | _ -> output
    intCodeClosure 0 ([])

let evaluateSystem system = 
     let tape = Utils.readLines "05" |> Seq.head |> getTape
     let output = intCode tape system
     output |> List.toSeq |> Seq.map string |> String.concat (", ")

let runDiagnose = evaluateSystem 1
