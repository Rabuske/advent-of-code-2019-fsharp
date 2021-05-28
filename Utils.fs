module Utils

let readLines (dayNumber:string) = 
    $"Inputs\\Day{dayNumber}.txt"
    |> System.IO.File.ReadLines

type Point2D(x:double, y:double) = 
    member this.x = x
    member this.y = y
    static member (-) (p1:Point2D, p2:Point2D) =
        Point2D(p1.x - p2.x, p1.y - p2.y)
    static member (+) (p1:Point2D, p2:Point2D) =
        Point2D(p1.x + p2.x, p1.y + p2.y)
    static member (*) (p1:Point2D, p2:Point2D) =
        Point2D(p1.x * p2.x, p1.y * p2.y)
    static member (*) (p:Point2D, m) =
        Point2D(p.x * m, p.y * m)
    static member manhattanDistance (p1:Point2D, p2:Point2D) = 
        abs(p1.x - p2.x) + abs(p1.y - p2.y)
    static member crossProduct (p1:Point2D, p2:Point2D) = 
        p1.x * p2.y - p1.y * p2.x

type LineSegment(p1:Point2D, p2:Point2D) = 
    member this.p1 = p1
    member this.p2 = p2
    static member coeficients(l: LineSegment) = 
        let a = l.p2.y - l.p1.y
        let b = l.p1.x - l.p2.x
        let c = a * l.p1.x + b * l.p1.y
        {|a = a; b = b; c = c |}

    static member isPointOnLine(l:LineSegment, p: Point2D) = 
        let isXOnLine = (l.p1.x <= p.x && p.x <= l.p2.x) || (l.p2.x <= p.x && p.x <= l.p1.x)
        let isYOnLine = (l.p1.y <= p.y && p.y <= l.p2.y) || (l.p2.y <= p.y && p.y <= l.p1.y)
        isXOnLine && isYOnLine

    static member intersectonPoint(l1: LineSegment, l2: LineSegment) = 
        let coeficients1 = LineSegment.coeficients l1
        let coeficients2 = LineSegment.coeficients l2
        let det = (coeficients1.a * coeficients2.b) - (coeficients2.a * coeficients1.b)
        if det = 0.0 then
            None // Line Segments are parallel
        else 
            let iX = ((coeficients2.b * coeficients1.c) - (coeficients1.b * coeficients2.c)) / det
            let iY = ((coeficients1.a * coeficients2.c) - (coeficients2.a * coeficients1.c)) / det
            let resultingPoint = Point2D(iX, iY);
            if LineSegment.isPointOnLine(l1, resultingPoint) && LineSegment.isPointOnLine(l2, resultingPoint) then
                Some(resultingPoint)
            else
                None 

let isOK (result:Result<_,_>) =
    match result with
        | Ok _ -> true
        | Error _-> false