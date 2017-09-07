//namespace vanmeule.FSharp.PlottingAndScheming

(* Copyright (c) 2009, 2008, 2007, 2006 by André van Meulebrouck.  All rights reserved worldwide. *)

//to do:  
//  bresenham algorithm for clipping:  
//      http://en.wikipedia.org/wiki/Bresenham's_line_algorithm 

module PlottingAndScheming.TwoD

open System.ComponentModel 
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Shapes

let arcoth (theta : float) =
    log (abs ((theta + 1.0) / (2.0 * (theta - 1.0))))

let hearts (theta : float) = 
    let tenTheta = 10.0 * theta
    let p1 = arcoth (2.0 * theta)
    let p2 =  2.0 * (sin (5.0 * theta) + (cos tenTheta))
    let p3 = sqrt (abs theta)
    let p4 = atan tenTheta
    p1 + p2 + p3 + p4

[<Measure>] type pixels
[<Measure>] type units

let distance (min : float) (max : float) = 
    max - min

let center (min : float) (max : float) =
    (distance min max) / 2.0 + min

//to do:  move to a utilities module

type coordinates = {
    centerX : float<units>;
    centerY : float<units>;
    unitDistanceX : float<units>;
    unitDistanceY : float<units>;
    pixelDistanceX : float<pixels>;
    pixelDistanceY : float<pixels>;
    upp : float<units/pixels>
    } with
    //get rid of call to centers
    //below:  deprecated, but used by extremas
    static member centers
        (xcenter : float<units>)
        (ycenter : float<units>)
        (xUnitDistance : float<units>)
        (yUnitDistance : float<units>)
        (xPixelDistance : float<pixels>)
        (yPixelDistance : float<pixels>) = 
        let xupp = 
            if xPixelDistance = 0.0<pixels> then 
                0.0<units/pixels> 
            else 
                xUnitDistance / xPixelDistance
        let yupp = 
            if yPixelDistance = 0.0<pixels> then
                0.0<units/pixels>
            else 
                yUnitDistance / yPixelDistance
        let upp = if xupp > yupp then xupp else yupp
        let xUnitDistance = upp * xPixelDistance
        let yUnitDistance = upp * yPixelDistance
        {   centerX = xcenter;
            centerY = ycenter;
            unitDistanceX = xUnitDistance;
            unitDistanceY = yUnitDistance;
            pixelDistanceX = xPixelDistance;
            pixelDistanceY = yPixelDistance; 
            upp = upp; }
    static member originFromPPU
        (xcenter : float<units>)
        (ycenter : float<units>)
        (ppu : float<pixels/units>)
        (xPixelDistance : float<pixels>)
        (yPixelDistance : float<pixels>) = 
        let xUnitDistance = xPixelDistance / ppu
        let yUnitDistance = yPixelDistance / ppu
        {   centerX = xcenter;
            centerY = ycenter;
            unitDistanceX = xUnitDistance;
            unitDistanceY = yUnitDistance;
            pixelDistanceX = xPixelDistance;
            pixelDistanceY = yPixelDistance; 
            upp = 1.0 / ppu; }
    static member originFromUPP
        (xcenter : float<units>)
        (ycenter : float<units>)
        (upp : float<units/pixels>)
        (xPixelDistance : float<pixels>)
        (yPixelDistance : float<pixels>) = 
        let xUnitDistance = upp * xPixelDistance
        let yUnitDistance = upp * yPixelDistance
        {   centerX = xcenter;
            centerY = ycenter;
            unitDistanceX = xUnitDistance;
            unitDistanceY = yUnitDistance;
            pixelDistanceX = xPixelDistance;
            pixelDistanceY = yPixelDistance; 
            upp = upp; }
    static member originFromUnits
        (xcenter : float<units>)
        (ycenter : float<units>)
        (units : float<units>)
        (xPixelDistance : float<pixels>)
        (yPixelDistance : float<pixels>) = 
        let uppX = units / xPixelDistance
        let uppY = units / yPixelDistance
        let upp = if uppX < uppY then uppX else uppY 
        let xUnitDistance = upp * xPixelDistance
        let yUnitDistance = upp * yPixelDistance
        {   centerX = xcenter;
            centerY = ycenter;
            unitDistanceX = xUnitDistance;
            unitDistanceY = yUnitDistance;
            pixelDistanceX = xPixelDistance;
            pixelDistanceY = yPixelDistance; 
            upp = upp; }
    static member extremas 
            (xmin : float<units>)
            (xmax : float<units>)
            (ymin : float<units>)
            (ymax : float<units>)
            (xpixels : float<pixels>)
            (ypixels : float<pixels>) =
        let (xcenter : float<units>) = (center (float xmin) (float xmax)) * 1.0<units>
        let (ycenter : float<units>) = (center (float ymin) (float ymax)) * 1.0<units>
        let (xunits : float<units>) = (distance (float xmin) (float xmax)) * 1.0<units>
        let (yunits : float<units>) = (distance (float ymin) (float ymax)) * 1.0<units>
        coordinates.centers xcenter ycenter xunits yunits xpixels ypixels
    member x.minX =
        x.centerX - (x.unitDistanceX / 2.0)
    member x.minY = 
        x.centerY - (x.unitDistanceY / 2.0)
    member x.maxX =
        x.centerX + (x.unitDistanceX / 2.0)
    member x.maxY =
        x.centerY + (x.unitDistanceY / 2.0)
    member x.unitToPixel (unit : float<units>) =
        unit * 1.0 / x.upp
    member x.unitToPixelX (unitX : float<units>) =
        x.unitToPixel (unitX - x.minX)
    member x.unitToPixelY (unitY : float<units>) =
        x.unitToPixel (x.maxY - unitY)
    member x.pixelToUnit (pixel : float<pixels>) =
        pixel * x.upp
    member x.maxPixelX () =
        float x.pixelDistanceX
    member x.maxPixelY () =
        float x.pixelDistanceY
    member x.polarToCartesian (theta : float) r =
        let ptX = r * (cos theta)
        let ptY = r * (sin theta)
        (ptX, ptY)
    member x.width = 
        int (float x.pixelDistanceX + 1.0)
    member x.height =
        int (float x.pixelDistanceY + 1.0)

let draw () = 
    let ln = new Line()
    ln.Stroke <- new SolidColorBrush(Colors.Black)
    ln.StrokeThickness <- 2.0
    ln

type point = {
    x : float;
    y : float}

type line = {
    pt1 : point;
    pt2 : point}

let walkX (coord : coordinates) f =
    let delta = float coord.upp
    let maxX = float coord.maxX
    let rec iter points x =
        if x > maxX then
            points
        else
            let y = f x
            let nextX = x + delta
            iter ({x = x; y = y} :: points) nextX
    let x = float coord.minX
    iter [] x

let walkTheta (coord : coordinates) f minTheta maxTheta delta =
    let rec iter points theta =
        if theta > maxTheta then
            points
        else
            let r = f theta
            let nextTheta = theta + delta
            let (x, y) = coord.polarToCartesian theta r
            iter ({x = x; y = y} :: points) nextTheta
    iter [] minTheta

let pointsToLines points =
    let rec iter lastPt acc pts =
        match pts with 
        | [] -> acc
        | x :: y -> 
            match lastPt with 
            | None -> iter (Some x) acc y
            | Some(z) -> iter (Some x) ({pt1 = lastPt.Value; pt2 = x} :: acc) y
    iter None [] points

let checkBounds (coords : coordinates) l =
    let maxX = float coords.maxX
    let maxY = float coords.maxY
    let minX = float coords.minX
    let minY = float coords.minY
    let inBounds x y = 
        (minX <= x && x <= maxX) && (minY <= y && y <= maxY)
    match l with
    | {pt1 = {x = x1; y = y1}; pt2 = {x = x2; y = y2}} ->
        (inBounds x1 y1) || (inBounds x2 y2)
    
let unitLinesToPixelLines lines (coords : coordinates) =
    List.map 
        (fun x -> 
            let {pt1 = {x = x1; y = y1}; pt2 = {x = x2; y = y2}} = x
            {   pt1 = {
                        x = (float (coords.unitToPixelX (x1 * 1.0<units>))); 
                        y = (float (coords.unitToPixelY (y1 * 1.0<units>)))}; 
                pt2 = {
                        x = (float (coords.unitToPixelX (x2 * 1.0<units>))); 
                        y = (float (coords.unitToPixelY (y2 * 1.0<units>)))}})
        lines

let linesToGuiLines lines =
    List.map 
        (fun x ->
            match x with
            | {pt1 = p1; pt2 = p2} ->
                let l = new Line()
                l.X1 <- p1.x
                l.Y1 <- p1.y
                l.X2 <- p2.x
                l.Y2 <- p2.y
                l.Stroke <- new SolidColorBrush(Colors.Black)
                l.StrokeThickness <- 2.0
                l)
        lines

//find extremas
       
let normalize (l : Line list) (coords : coordinates) =
    let maxPixelX = coords.maxPixelX ()
    let maxPixelY = coords.maxPixelY ()
    let rec iter oldlines newlines =         
        match oldlines with
        | [] -> newlines
        | x :: y -> 
            //see if the point is in bounds
            let (ln : Line) = x
            if ln.X1 < 0.0 then ln.X1 <- 0.0                        
            if ln.X1 > maxPixelX then ln.X1 <- maxPixelX
            if ln.Y1 < 0.0 then ln.Y1 <- 0.0
            if ln.Y1 > maxPixelY then ln.Y1 <- maxPixelY
            if ln.X2 < 0.0 then ln.X2 <- 0.0
            if ln.X2 > maxPixelX then ln.X2 <- maxPixelX
            if ln.Y2 < 0.0 then ln.Y2 <- 0.0
            if ln.Y2 > maxPixelY then ln.Y2 <- maxPixelY
            iter y (ln :: newlines) 
    iter l []

let plot2d () =
    let coords = 
        coordinates.centers
            0.0<units>
            0.0<units>
            20.0<units>
            20.0<units>
            500.0<pixels>
            500.0<pixels>
    let worker = new BackgroundWorker()
    worker.DoWork.Add(fun args ->
        //let points = walkX coords tan
        let points = walkTheta coords hearts 0.8 920.0 0.01
        let lines = pointsToLines points
        //filter
        //let lines = List.filter (fun x -> checkBounds coords x) lines
        let lines = unitLinesToPixelLines lines coords        
        args.Result <- lines)
    worker.RunWorkerCompleted.Add(fun args ->
        //let lines = normalize lines coords
        let can = new Canvas()
        can.Width <- float coords.pixelDistanceX
        can.Height <- float coords.pixelDistanceY
        let lines = args.Result :?> line list
        let lines = linesToGuiLines lines
        for el in lines do
            can.Children.Add el |> ignore
        let win = new Window()
        win.SizeToContent <- SizeToContent.WidthAndHeight
        win.Content <- can 
        win.Show() |> ignore)
    worker.RunWorkerAsync()

//let populateCbx (cbx : ComboBox) = //to do:  take xml location as argument, get from AppConfig

//to do:  have plot code to hook up to button come from here

//(define (arcoth theta)
//  (log (abs (/ (+ theta 1)
//          (* 2 (- theta 1))))))
//;
//
//(define (hearts theta)
//  (let ((ten-theta (* 10 theta)))
//    (+ (arcoth (* 2 theta))
//       (* 2 (+ (sin (* 5 theta))
//               (cos ten-theta)))
//       (sqrt (abs theta))
//       (atan ten-theta 1))))
//; 
//(define polar-hearts-plot (make-2d-polar-plot-object hearts .8 26 300))