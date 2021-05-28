module Day03

// This first attempt will generate the entire path of the wire
type wire = {
    path: Utils.Point2D list
}

let determinePath (currentPosition: Utils.Point2D) (directionAndSteps:string) = 
    let direction = directionAndSteps.[0]
    let steps = directionAndSteps.Remove(0,1) |> double
    match direction with
        | 'R' -> currentPosition + Utils.Point2D(steps, 0.0)
        | 'L' -> currentPosition - Utils.Point2D(steps, 0.0)
        | 'U' -> currentPosition + Utils.Point2D(0.0, steps)
        | 'D' -> currentPosition - Utils.Point2D(0.0, steps)

let getWire (directionsAndSteps: string[]) = 
    let mutable currentPosition = Utils.Point2D(0.0, 0.0)
    let wireWithout00 = 
        directionsAndSteps 
            |> Array.toList
            |> List.map(fun(directionAndSteps) -> 
                currentPosition <- determinePath currentPosition directionAndSteps
                Utils.Point2D(currentPosition.x, currentPosition.y)
            )
    { path = List.append [Utils.Point2D(0.0,0.0)] wireWithout00 }

let getCrossingPoints (wire1:wire) (wire2:wire) = 
    seq {
        for wire1PointIndex in [0..(wire1.path.Length - 2)] do
            for wire2PointIndex in [0..(wire2.path.Length - 2)] do
                let LineSegment1 = Utils.LineSegment(wire1.path.[wire1PointIndex] , wire1.path.[wire1PointIndex + 1])
                let LineSegment2 = Utils.LineSegment(wire2.path.[wire2PointIndex], wire2.path.[wire2PointIndex + 1])
                let crossingPoint = Utils.LineSegment.intersectonPoint(LineSegment1, LineSegment2);
                if crossingPoint.IsSome then
                    crossingPoint.Value
    } |> Seq.filter(fun(point) -> point.x <> 0.0 || point.y <> 0.0)

let buildLinesToPoint point wire =     
    seq {
        let mutable pointFound = false
        let mutable index = 0
        while not pointFound do
            let line = Utils.LineSegment(wire.path.[index], wire.path.[index+1])
            index <- index + 1
            pointFound <- Utils.LineSegment.isPointOnLine(line, point)
            if pointFound then
                Utils.LineSegment(line.p1, point)
            else
                line            
    } |> Seq.toList

let calculateWalkingDistance (lines:Utils.LineSegment list) =
    lines
    |> List.map(fun(line) ->
        Utils.Point2D.manhattanDistance(line.p1, line.p2)
    ) |> List.sum    

let getSumOfStepsUntilPoints (wires:wire list) (point:Utils.Point2D) = 
    wires
        |> List.map(buildLinesToPoint point)
        |> List.map(calculateWalkingDistance) 
        |> List.sum

let processInputPart2 = 
    let LineSegments = Utils.readLines "03"
    let wire1 = getWire ((Seq.item 0 LineSegments).Split(','))
    let wire2 = getWire ((Seq.item 1 LineSegments).Split(','))
    getCrossingPoints wire1 wire2 
    |> Seq.map(getSumOfStepsUntilPoints [wire1; wire2])
    |> Seq.min
    
let processInputPart1 = 
    let LineSegments = Utils.readLines "03"
    let wire1 = getWire ((Seq.item 0 LineSegments).Split(','))
    let wire2 = getWire ((Seq.item 1 LineSegments).Split(','))
    getCrossingPoints wire1 wire2 
    |> Seq.map(fun(point) -> Utils.Point2D.manhattanDistance(point, Utils.Point2D(0.0,0.0)))
    |> Seq.min