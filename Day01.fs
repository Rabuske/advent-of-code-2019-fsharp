module Day01

let fuelRequired (_: int) (mass:int) = int (float mass / 3.0) |> fun(x) -> x - 2
    
let rec fuelRequiredRecursive (totalFuel:int) (mass:int) = 
    fuelRequired 0 mass
    |> function
        | x when x <= 0 -> totalFuel
        | x -> fuelRequiredRecursive (totalFuel + x) x 

let processInput (fuelRequirement:int->int->int) = 
    Utils.readLines "01"
    |> Seq.map int
    |> Seq.map(fuelRequirement 0)
    |> Seq.sum