module Day06

type PlanetAndOrbit = {
    Planet: string;
    Orbit: string
}

let parseIntoOrbits (line:string) =
    let obj = line.Split(")")
    obj.[1], obj.[0]

let buildPlanetsDictionary lines = 
    lines 
        |> Seq.map parseIntoOrbits 
        |> Map.ofSeq

let pathToRoot (orbits: Map<string, string>) (planet: string) =
    let rec pathToRootClosure planet = 
        match Map.tryFind planet orbits with
            | Some p -> p :: pathToRootClosure p
            | None -> [ ]
    pathToRootClosure planet

let processPart01 = 
    let orbits = buildPlanetsDictionary <| Utils.readLines "06"
    orbits |> Map.toSeq |> Seq.sumBy (fst >> pathToRoot orbits >> List.length)

let processPart02 = 
    let orbits = buildPlanetsDictionary <| Utils.readLines "06"
    let youToRoot = pathToRoot orbits "YOU" |> List.rev
    let santaToRoot = pathToRoot orbits "SAN" |> List.rev
    
    let commomDistance = Seq.zip youToRoot santaToRoot |> Seq.findIndex (fun (planet1, planet2) -> planet1 <> planet2)
    youToRoot.Length + santaToRoot.Length - (2 * commomDistance)