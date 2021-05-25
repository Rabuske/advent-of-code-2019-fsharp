module Day02

let getTape (inputLine:string) =
    inputLine.Split(",")
    |> Array.map int

let fixEntries noun verb (tape:int[]) = 
    tape.[1] <- noun
    tape.[2] <- verb
    tape

let applySingleOperationToTape (tape:int[]) (currentPosition:int) (operation:int->int->int) = 
    let positionNumber1 = tape.[currentPosition+1]
    let positionNumber2 = tape.[currentPosition+2]
    let positionResult = tape.[currentPosition+3]
    tape.[positionResult] <- operation tape.[positionNumber1]  tape.[positionNumber2]
    
let rec executeOperationsStartingAtPosition (position:int) (tape:int[]) = 
    match tape.[position] with
        | 1 -> 
            applySingleOperationToTape tape position (+)
            executeOperationsStartingAtPosition (position+4) tape
        | 2 -> 
            applySingleOperationToTape tape position (*)
            executeOperationsStartingAtPosition (position+4) tape
        | 99 -> tape
        | _ -> Array.empty

let findNumberThatProducesOutput (number:int) (tape:int[]): int = 
    seq {
        for noun in [1..99] do
            for verb in [1..99] do
                let clonedtape = Array.copy tape
                fixEntries noun verb clonedtape |> ignore
                executeOperationsStartingAtPosition 0 clonedtape |> ignore
                if clonedtape.[0] = number then
                    (100 * noun) + verb    
    } |> Seq.head
    
let processInputPart1 = 
    Utils.readLines "02"
    |> Seq.head 
    |> getTape 
    |> fixEntries 12 2
    |> executeOperationsStartingAtPosition 0
    |> Array.head    

let processInputPart2: int = 
    Utils.readLines "02"
    |> Seq.head 
    |> getTape 
    |> findNumberThatProducesOutput 19690720    
