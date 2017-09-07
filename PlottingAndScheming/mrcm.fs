module PlottingAndScheming.MRCM

open System.Windows
open System.Windows.Controls 
open System.Windows.Media
open System.Windows.Shapes

type rectangle = {
    left : float;
    top : float; 
    width : float;
    height : float;
    brush : Brush;
    level : int;
    final :  bool;
    } with 
    static member create left top width height brush level = {
        left = left;
        top = top;
        width = width;
        height = height;
        brush = brush;
        level = level; 
        final = false; }

let rectangleToWpf (r : rectangle) =
    let newR = new Rectangle ()
    newR.Width <- r.width
    newR.Height <- r.height
    newR.SetValue (Canvas.LeftProperty, r.left)
    newR.SetValue (Canvas.TopProperty, r.top)
    newR.Fill <- r.brush
    newR.SetValue (Canvas.ZIndexProperty, r.level)
    newR

let mrcm shape generation =
    let rec recur level candidates excludes = 
        if level = generation then
            let shapes = List.map rectangleToWpf candidates
            let can = new Canvas ()
            for s in shapes do
                can.Children.Add s |> ignore
            let win = new Window ()
            win.Content <- can
            //win.SizeToContent <- SizeToContent.WidthAndHeight
            win.Show ()
            //convert growing and completed into WPF controls
            //place in window
            //show window
            //()
        else 
            //divide square into 9 squares
            let newCandidates = 
                let rec iter candidates excludes acc =
                    match candidates with 
                    | [] -> acc
                    | h :: t -> 
                        let newWidth = h.width / 3.0
                        let height = newWidth
                        let left = h.left
                        //1
                        let newLeft = left //+ newWidth
                        let newTop = h.top
                        let brush = new SolidColorBrush (Colors.Blue)
                        let newRec = rectangle.create newLeft newTop newWidth height brush level
                        let acc = newRec :: acc
                        //2
                        let newLeft = newLeft + newWidth
                        let newRec = rectangle.create newLeft newTop newWidth height brush level
                        let acc = newRec :: acc
                        let newRec = rectangle.create newLeft newTop newWidth height brush level
                        let acc = newRec :: acc
                        //3
                        let newLeft = newLeft + newWidth
                        let newRec = rectangle.create newLeft newTop newWidth height brush level
                        let acc = newRec :: acc
                        //4
                        let newRec = rectangle.create newLeft newTop newWidth height brush level
                        let acc = newRec :: acc
                        //5
                        //let newLeft = newLeft + newWidth
                        let newTop = newTop + newWidth
                        let newRec = rectangle.create newLeft newTop newWidth height brush level
                        let acc = newRec :: acc
                        //6
                        let newRec = rectangle.create newLeft newTop newWidth height brush level
                        let acc = newRec :: acc
                        //7
                        //let newLeft = newLeft + newWidth
                        let newLeft = left
                        let newRec = rectangle.create newLeft newTop newWidth height brush level
                        let acc = newRec :: acc
                        //8
                        let newTop = newTop + newWidth
                        let brush = new SolidColorBrush (Colors.Blue)
                        let newRec = rectangle.create newLeft newTop newWidth height brush level
                        let acc = newRec :: acc
                        //9
                        let newLeft = newLeft + newWidth
                        let newRec = rectangle.create newLeft newTop newWidth height brush level
                        let acc = newRec :: acc
                        let newRec = rectangle.create newLeft newTop newWidth height brush level
                        let acc = newRec :: acc
                        //10
                        let newLeft = newLeft + newWidth
                        let newRec = rectangle.create newLeft newTop newWidth height brush level
                        let acc = newRec :: acc
                        iter t excludes acc
                iter candidates excludes []
            recur (level + 1) newCandidates excludes
    recur 0 [shape] []

let plotFunc = 
    (fun _ ->
        let shape = rectangle.create 0.0 0.0 200.0 200.0 (new SolidColorBrush (Colors.Blue)) 0
        mrcm shape 3)
