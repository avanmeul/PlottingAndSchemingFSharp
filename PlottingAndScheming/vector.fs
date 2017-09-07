//namespace vanmeule.FSharp.PlottingAndScheming

(* Copyright (c) 2009, 2008, 2007, 2006 by André van Meulebrouck.  All rights reserved worldwide. *)

//new vs. old
module PlottingAndScheming.Vector

open PlottingAndScheming.Scheme
open PlottingAndScheming.xml
open System
open System.Collections.Generic
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Shapes
open System.Xml.Linq

(*
to do:

0) lambda closure problem
1) back up to old
2) convert drawf into record
3) add counter to it
4) add coloring algorithm based on vector line count (using modulo math)
5) work out the math to generalize it
6) force:  force a certain color
7) filter out duplicate vectors
9) clean up lisp code
11) sort combo box entries
12) status pane for status information with tabs (warnings, errors, info)
    disable tabs if empty
    use info for window title
13) hilbert (can't do this one using, except maybe via adding generation parameter)

*)

type palettePicker = {
    palette : Color array; 
    mutable loc : int; } with
    static member create (p : Color array) = {        
        palette = 
            if p.Length = 0 then
                failwith "the palette cannot be empty"
            else p;
        loc = -1; }
    member x.color 
        with get () =
            if x.loc > -1 then
                x.palette.[x.loc]
            else 
                failwith "attempt to reference color when color wasn't set"
    member x.index 
        with get () = 
            x.loc
        and set i =
            if i < 0 then
                failwith "index cannot be negative:  for -1, use reset"
            else
                x.loc <- i % x.palette.Length
    member x.inc () =
        x.loc <- (x.loc + 1) % x.palette.Length
    member x.reset () =
        x.loc <- -1
        
//to do:  could put start index into the mix, retrieve from xml
type sizeColorizer = {
    numberOfRules : int;
    picker : palettePicker;
    subtractor : int;
    generations : int;
    ruleCount : int;
    mutable sizes : double list;
    mutable counter : int;
    } with
    static member create numRules picker subtractor generations = 
        let exp = generations - subtractor
        if exp < 0 then
            failwith "subtractor should not exceed generations"
        else {
            ruleCount = numRules;
            numberOfRules = pown numRules (generations - subtractor);
            picker = picker;
            subtractor = subtractor;
            generations = generations;
            sizes = [];
            counter = 0; }
    member x.check i size =        
        if i % x.numberOfRules = 0 then 
            //check to see if we know about this size already
            let find = List.tryFindIndex (fun el -> el = size) x.sizes
            match find with 
            | Some i -> 
                //use size index to set the index of the color picker
                x.picker.index <- i
            | None -> 
                //add newly found size
                x.sizes <- x.sizes @ [size]
                x.picker.index <- x.sizes.Length - 1

type levelColorizer = {
    picker : palettePicker; 
    level : int; } with
    static member create (picker : palettePicker) level generations = {
        picker = picker;
        level = level % (generations + 1) }
    member x.check level =
        if x.level = level then
            x.picker.inc ()

type vectorColorizer = 
    | Level of levelColorizer
    | Image of sizeColorizer
    //| Solid of int
    //| Group of int
    //| Force

//to do:  push function
//keep track of number of line segments
//coloring algorithm
//first, just match existing functionality
type drawf = {
    lines : Stack<Line>;
    color : Color; 
    //stroke thickness
    } with 
    static member create () = 
        ()
    member x.push x1 y1 x2 y2 = 
        ()
    
type vecLen = //length -> computed length
    | BuiltIn of (double -> double)
    | Scheme of scmBlock
type vecAngle = //angle -> flip rule -> computed angle
    | BuiltIn of (double -> int -> double) 
    | Scheme of scmBlock
//to do:  should be length -> angle -> flip -> origin -> computed origin
type vecOrigin = //length -> angle -> origin -> flip -> computed origin
    | BuiltIn of (double -> double -> (double * double) -> int -> (double * double)) 
    | Scheme of scmBlock
type vecFlip =
    | BuiltIn of int
    | Scheme of scmAtom
    
type vectorRule = {
    lenf : vecLen;
    anglef : vecAngle;
    originf : vecOrigin;
    flipAngle : vecFlip;
    flipRules : vecFlip; }

//to do:
(*
step 2:  parse rules from xml
step 3:  create scheme rules
step 4:  change plotter to match on type of rule
*)

let seed, rules = 
    let identity x = x
    let project1of2 x y = x
    let project3of4 w x (y : (double * double)) z = y
    let lenDiv3 len = len / 3.0
    let degrees60 = Math.PI / 3.0
    let anglePlus60 angle flip = angle + (degrees60 * (double flip))
    let angleLess60 angle flip = angle - (degrees60 * (double flip))
    let fivePiOver6 = 5.0 * Math.PI / 6.0
    let anglePlus5PiOver6 angle flip = angle + (fivePiOver6 * (double flip))
    let angleLess5PiOver6 angle flip = angle - (fivePiOver6 * (double flip))
    let lenDiv3sqrt3 len = len / (3.0 * sqrt 3.0)
    let angleLessPiHalves angle flip = angle - ((double flip) * Math.PI / 2.0)                
    let initiator = [ 
        //(list identity project1.2 project3.4 1 1)
         {  lenf = vecLen.BuiltIn identity;
            anglef = vecAngle.BuiltIn project1of2; 
            originf = vecOrigin.BuiltIn project3of4; 
            flipAngle = vecFlip.BuiltIn 1;
            flipRules = vecFlip.BuiltIn 1;
            }]  
    let generator = [
        //rule 1 (list len-div-3 angle-plus-60 project3.4 -1 1) ; 1
        {   lenf = vecLen.BuiltIn lenDiv3; 
            anglef = vecAngle.BuiltIn anglePlus60;
            originf = vecOrigin.BuiltIn project3of4;
            flipAngle = vecFlip.BuiltIn -1;
            flipRules = vecFlip.BuiltIn 1; 
            };
        //rule 2 (list len-div-3 angle-plus-60 project3.4 1 1) ; 2
        {   lenf = vecLen.BuiltIn lenDiv3; 
            anglef = vecAngle.BuiltIn anglePlus60;
            originf = vecOrigin.BuiltIn project3of4;
            flipAngle = vecFlip.BuiltIn 1;
            flipRules = vecFlip.BuiltIn 1;  
            };
        //rule 3 (list len-div-3 project1.2 project3.4 1 1) ; 3
        {   lenf = vecLen.BuiltIn lenDiv3; 
            anglef = vecAngle.BuiltIn project1of2;
            originf = vecOrigin.BuiltIn project3of4;
            flipAngle = vecFlip.BuiltIn 1;
            flipRules = vecFlip.BuiltIn 1; 
            };
        //rule 4 (list len-div-3 angle-less-60 project3.4 1 1) ; 4
        {   lenf = vecLen.BuiltIn lenDiv3; 
            anglef = vecAngle.BuiltIn angleLess60;
            originf = vecOrigin.BuiltIn project3of4;
            flipAngle = vecFlip.BuiltIn 1;
            flipRules = vecFlip.BuiltIn 1;  
            };
        //rule 5 (list len-div-3sqrt3 angle-plus-5pi-over-6 project3.4 1 1) ; 5
        {   lenf = vecLen.BuiltIn lenDiv3sqrt3; 
            anglef = vecAngle.BuiltIn anglePlus5PiOver6;
            originf = vecOrigin.BuiltIn project3of4;
            flipAngle = vecFlip.BuiltIn 1;
            flipRules = vecFlip.BuiltIn 1;  
            };
        //rule 6 (list len-div-3sqrt3 angle-plus-5pi-over-6 project3.4 -1 1) ; 6
        {   lenf = vecLen.BuiltIn lenDiv3sqrt3; 
            anglef = vecAngle.BuiltIn anglePlus5PiOver6;
            originf = vecOrigin.BuiltIn project3of4;
            flipAngle = vecFlip.BuiltIn -1;
            flipRules = vecFlip.BuiltIn 1; 
            };
        //rule 7 (list len-div-3sqrt3 angle-less-5pi-over-6 project3.4 -1 1) ; 7
        {   lenf = vecLen.BuiltIn lenDiv3sqrt3; 
            anglef = vecAngle.BuiltIn angleLess5PiOver6;
            originf = vecOrigin.BuiltIn project3of4;
            flipAngle = vecFlip.BuiltIn -1;
            flipRules = vecFlip.BuiltIn 1; 
            };
        //rule 8 (list len-div-3sqrt3 angle-less-pi-halves project3.4 -1 1) ; 8
        {   lenf = vecLen.BuiltIn lenDiv3sqrt3; 
            anglef = vecAngle.BuiltIn angleLessPiHalves;
            originf = vecOrigin.BuiltIn project3of4;
            flipAngle = vecFlip.BuiltIn -1;
            flipRules = vecFlip.BuiltIn 1; 
            };
        //rule 9 (list len-div-3 project1.2 project3.4 1 1) ; 9
        {   lenf = vecLen.BuiltIn lenDiv3; 
            anglef = vecAngle.BuiltIn project1of2;
            originf = vecOrigin.BuiltIn project3of4;
            flipAngle = vecFlip.BuiltIn 1;
            flipRules = vecFlip.BuiltIn 1; 
            };
        //rule 10 (list len-div-3sqrt3 angle-less-5pi-over-6 project3.4 1 1) ; 10
        {   lenf = vecLen.BuiltIn lenDiv3sqrt3; 
            anglef = vecAngle.BuiltIn angleLess5PiOver6;
            originf = vecOrigin.BuiltIn project3of4;
            flipAngle = vecFlip.BuiltIn 1;
            flipRules = vecFlip.BuiltIn 1; 
            };
        //rule 11 (list len-div-3sqrt3 angle-less-5pi-over-6 project3.4 -1 1) ; 11
        {   lenf = vecLen.BuiltIn lenDiv3sqrt3; 
            anglef = vecAngle.BuiltIn angleLess5PiOver6;
            originf = vecOrigin.BuiltIn project3of4;
            flipAngle = vecFlip.BuiltIn -1;
            flipRules = vecFlip.BuiltIn 1;
            };
        //rule 12 (list len-div-3 project1.2 project3.4 -1 1) ; 12
        {   lenf = vecLen.BuiltIn lenDiv3; 
            anglef = vecAngle.BuiltIn project1of2;
            originf = vecOrigin.BuiltIn project3of4;
            flipAngle = vecFlip.BuiltIn -1;
            flipRules = vecFlip.BuiltIn 1; 
            };
        //rule 13 (list len-div-3 project1.2 project3.4 1 1)) ; 13
        {   lenf = vecLen.BuiltIn lenDiv3; 
            anglef = vecAngle.BuiltIn project1of2;
            originf = vecOrigin.BuiltIn project3of4;
            flipAngle = vecFlip.BuiltIn 1;
            flipRules = vecFlip.BuiltIn 1; 
            };
            ]
    initiator, generator

let xmlPalette (xml : XElement) =
    xml.Elements ()
    |> Seq.map                    
        (fun e -> 
            let el = e.FirstNode :?> XElement
            let name = el.Name.LocalName
            match name with
            | "name" -> 
                ColorConverter.ConvertFromString(el.Value) :?> Color
            | "rgb" ->
                let r = el.Element (xname "red")
                let g = el.Element (xname "green")
                let b = el.Element (xname "blue")
                let r = Convert.ToByte (r.Value)
                let g = Convert.ToByte (g.Value)
                let b = Convert.ToByte (b.Value)
                let clr = Color.FromRgb(r, g, b)
                clr
            | _ -> 
                failwith "bad color specification") 
    |> Seq.toArray

type plotObject = {
    xml : XElement; 
    initiator : vectorRule list;
    generator : vectorRule list; 
    generations : int; 
    length : float; 
    coloring : vectorColorizer; 
    continuous : bool; } with 
    static member create (xml : XElement) =
        let parameters = xml.Element (xname "parameters")
        let generations = PlottingAndScheming.xml.fetchInt parameters "generations"
        let generations = 
            match generations with
            | Some i -> i
            | _ -> failwith "bad generations specified in xml"
        let len = PlottingAndScheming.xml.fetchDouble parameters "length"
        let len = 
            match len with
            | Some f -> f
            | _ -> failwith "bad length specified in xml"
        let coloring = PlottingAndScheming.xml.fetchElement parameters "coloring"
        let palette = PlottingAndScheming.xml.fetchElement coloring "palette"
        let colors = xmlPalette palette
        let algorithm = PlottingAndScheming.xml.fetchElement coloring "algorithm"
        let algorithmType = PlottingAndScheming.xml.fetchAttribute algorithm "type"
        let picker = palettePicker.create colors
        let code = xml.Element (xname "rules")
        let builtin = code.Attribute (xname "type")
        let builtin = 
            if builtin = null then
                "builtin"
            else 
                builtin.Value.ToString ()
        let continuous = 
            let att = code.Attribute (xname "continuous") 
            if att = null then
                true
            else 
                (att.Value.ToString ()) = "yes"  
        let seed, rules =
            match builtin with
            | "builtin" -> 
                seed, rules
            | _ -> 
                let code = code.Value
                let code = code.Replace("\n", "\r\n")               
                let evaled = evalString code
                let dump = printHeap evaled
                let e = Some evaled
                let initiator, generator =
                    funCar e, funCadr e
                //break up LISP expressions into F# records
                //walk down into rules
                let createRules rules = 
                    let rec iter rules records =
                        if funNull rules then
                            List.rev records
                        else
                            let rule = Some (funCar rules)
                            //walk across a rule
                            let lenf = funCar rule
                            let rule = funCdr rule
                            let anglef = funCar rule
                            let rule = funCdr rule
                            let originf = funCar rule
                            let rule = funCdr rule
                            let flipAngle = funCar rule
                            let flipa = (fromScheme flipAngle) :?> int               
                            let rule = funCdr rule
                            let flipRules = funCar rule
                            let flipr = (fromScheme flipRules) :?> int
                            let record = 
                                match lenf, anglef, originf, flipAngle, flipRules with
                                |   scmObject.Block l, 
                                    scmObject.Block a,
                                    scmObject.Block o,
                                    scmObject.Atom (scmAtom.Int ia),
                                    scmObject.Atom (scmAtom.Int ir) -> 
                                    {   lenf = vecLen.Scheme l;
                                        anglef = vecAngle.Scheme a;
                                        originf = vecOrigin.Scheme o;
                                        //originf = vecOrigin.BuiltIn (fun l a o f -> o);
                                        flipAngle = 
                                            vecFlip.BuiltIn flipa;
                                        flipRules = 
                                            vecFlip.BuiltIn flipr;
                                        }
                                | _ -> failwith "bad rule specification"
                            iter (funCdr rules) (record :: records)
                    iter rules []
                let seed = createRules (Some initiator)
                let rules = createRules (Some generator)
                seed, rules
        let colorizer =
            match algorithmType with
            | Some "image" -> 
                let subtractor = 
                    let el = PlottingAndScheming.xml.fetchInt algorithm "subtractor"
                    match el with
                    | Some i -> i
                    | None -> failwith "subtractor was not specified for images coloring algorithm"
                vectorColorizer.Image 
                    (sizeColorizer.create rules.Length picker subtractor generations)
            | Some "level" ->
                let level = 
                    let el = PlottingAndScheming.xml.fetchInt algorithm "level"
                    match el with
                    | Some i -> i
                    | None -> failwith "bad level argument for level colorizer"
                vectorColorizer.Level (levelColorizer.create picker level generations)
            | _ -> failwith "unknown colorizing algorithm"
        {   xml = xml; 
            initiator = seed;
            generator = rules; 
            generations = generations; 
            length = len; 
            coloring = colorizer; 
            continuous = continuous; } 

let vectorEndPoint vec len angle =
    (fst vec) + (len * (Math.Cos angle)),
    (snd vec) + (len * (Math.Sin angle))

let vectorFractal (plotobj : plotObject) =
    let seed = plotobj.initiator
    let rules = plotobj.generator
    let colorizer = plotobj.coloring 
    let drawf 
        (lines : Stack<Line>) 
        (len : double) 
        (angle : double)
        (origin : double * double) =
        let x = fst origin
        let y = snd origin
        let ln = new Line()
        ln.X1 <- x
        ln.Y1 <- y
        let x2 = x + len * Math.Cos angle
        let y2 = y + len * Math.Sin angle
        ln.X2 <- x2
        ln.Y2 <- y2
        let idx = lines.Count
        let chk p i j n = i >= n * p && i < (n + j) * p
        let gen = plotobj.generations
        (*
        There are two coloring cases: 
            1) the same diagram repeats at different sizes
            2) all diagrams are the same size

        For coloring type #1, look for the sizes of diagrams and color based on size.
        For type #2, decide how many groups you want and color accordingly
        *)
        let clr = 
            match colorizer with 
            | vectorColorizer.Image i ->
                i.check idx len
                i.picker.color
            | vectorColorizer.Level l ->
                l.picker.color               
        ln.Stroke <- new SolidColorBrush(clr)
        ln.StrokeThickness <- 2.0
        lines.Push ln
        (x2, y2)
    let twoDvectorFractal 
        drawf 
        (seed : list<vectorRule>) 
        (rules : list<vectorRule>) 
        (len : double)
        (angle : double)
        (xorigin : double)
        (yorigin : double)
        (quitf : double -> int -> bool)
        =  
        //change mapvector to be non-recursive 
        //it will record the end of the vector
        //it will return this end of the vector when the vector is done being walked
        //inside mapvector will be a recursive function that will do the walking
        //do a backup before doing this!!!!
        //to do:  instead of len, angle, origin; create a vector class and use real vectors      
        let rec across 
            len 
            angle 
            (origin : double * double) 
            (flipAngleFactor : int) //to do:  change to int
            (flipRulesFactor : int) //to do:  change to int
            generation
            (vector : list<vectorRule>) 
            =            
            if vector.IsEmpty then 
                origin                
            else 
                let (currentSeed : vectorRule) = List.head vector
                let restSeed = List.tail vector
                let lenf =
                    match currentSeed.lenf with
                    | vecLen.BuiltIn f -> 
                        (f len)
                    | vecLen.Scheme g -> 
                        let res = apply g ((toScheme len) :: [])
                        let res = fromScheme res
                        let res =
                            match res with
                            | :? int as i -> failwith "wanted a double"
                            | :? double as d -> d
                            | _ -> failwith "bad result from Scheme"
                        res
                let anglef = 
                    match currentSeed.anglef with
                    | vecAngle.BuiltIn f -> 
                        (f angle flipAngleFactor)
                    | vecAngle.Scheme s -> 
                        let args = (toScheme angle) :: (toScheme flipAngleFactor) :: []
                        let res = apply s args
                        let res = fromScheme res
                        let res =
                            match res with
                            | :? double as d -> d
                            | :? int as i -> 
                                (double i)
                            | _ -> failwith "bad result from Scheme"
                        res
                let originf =                    
                    let newOrigin =                             
                        across 
                            len 
                            angle 
                            origin 
                            flipAngleFactor
                            flipRulesFactor
                            generation 
                            restSeed
                    match currentSeed.originf with
                    | vecOrigin.BuiltIn f -> 
                        (f                       
                            len 
                            angle
                            newOrigin
                            flipAngleFactor)
                    | vecOrigin.Scheme g -> 
                        let args = 
                            (toScheme len) ::
                            (toScheme angle) ::
                            (toScheme newOrigin) :: 
                            (toScheme flipAngleFactor) :: 
                            []
                        let res = apply g args
                        let res = fromScheme res
                        let res =
                            match res with
                            | :? int as i -> failwith "wanted a double * double"
                            | :? double as d -> failwith "wanted a double * double"
                            | :? (double * double) as t -> t
                            | _ -> failwith "bad result from Scheme"
                        res
                let flipAngle = 
                    match currentSeed.flipAngle with
                    | vecFlip.BuiltIn d -> d
                    | vecFlip.Scheme s -> failwith "not implemented yet"
                let flipRules = 
                    match currentSeed.flipRules with
                    | vecFlip.BuiltIn d -> d
                    | vecFlip.Scheme s -> failwith "not implemented yet"
                down 
                    lenf
                    anglef
                    originf
                    (flipAngleFactor * flipAngle)
                    (flipRulesFactor * flipRules)
                    (generation + 1) 
        and 
            down 
                len 
                angle 
                (origin : double * double) 
                flipAngleFactor 
                flipRulesFactor 
                generation 
                =
            match colorizer with
                | vectorColorizer.Level l ->
                    l.check (int generation)
                    ()
                | _ -> 
                    ()
            if quitf len generation then
                (drawf 
                    len 
                    angle 
                    origin)
            else 
                let rules = 
                    if flipRulesFactor > 0 then 
                        (List.rev rules)
                    else
                        rules
                let newOrigin =
                    across
                        len
                        angle
                        origin
                        flipAngleFactor
                        flipRulesFactor
                        generation
                        rules
                if plotobj.continuous then
                    newOrigin
                else 
                    vectorEndPoint origin len angle
        across 
            len 
            angle 
            (xorigin, yorigin) //origin
            1 //angle flip
            1 //rule flip
            -1 //generation
            (List.rev seed)
    let quitf generations = 
        if generations < 0 then
            fun (len : double) (generation : int) -> len < (double generations)
        else 
            fun (len : double) (generation : int) -> generation = generations    
    let win = new Window()
    win.SizeToContent <- SizeToContent.WidthAndHeight
    let xml = plotobj.xml
    let title = 
        let info = xml.Element (xname "info")
        if info <> null then 
            let name = info.Element (xname "name")
            match name, plotobj.generations with 
            | null, _ -> failwith "couldn't find the name"
            | n, g -> n.Value + " [generation=" + g.ToString () + "]"
        else
            failwith "couldn't find the info node" 
    win.Title <- title
    let lines = new Stack<Line>()
    let clr = Colors.Black
    let generations = plotobj.generations
    let len = plotobj.length
    let _ = 
        twoDvectorFractal 
            (drawf lines)             
            seed //initiator
            rules //generator
            len //len
            0.0 //start angle
            0.0 //flip angle
            0.0 //flip rules
            (quitf generations)
    let can = new Canvas()  
    //find extrema
    let line = lines.Peek()
    let loX : float ref = ref line.X1
    let hiX : float ref = ref line.X1
    let loY : float ref = ref line.Y1
    let hiY : float ref = ref line.Y1
    let checker x y =
        if x < loX.Value then loX.Value <- x
        if x > hiX.Value then hiX.Value <- x
        if y < loY.Value then loY.Value <- y
        if y > hiY.Value then hiY.Value <- y
    for line in lines do
        checker line.X1 line.Y1
        checker line.X2 line.Y2
    //set canvas size
    can.Width <- hiX.Value - loX.Value
    can.Height <- hiY.Value - loY.Value           
    for line in lines do
        //normalize lines
        line.X1 <- line.X1 - loX.Value 
        line.Y1 <- hiY.Value - line.Y1
        line.X2 <- line.X2 - loX.Value
        line.Y2 <- hiY.Value - line.Y2
        //add line to canvas
        can.Children.Add line |> ignore
    let scroll = ScrollViewer()
    scroll.Content <- can
    scroll.HorizontalScrollBarVisibility <- ScrollBarVisibility.Auto
    scroll.VerticalScrollBarVisibility <- ScrollBarVisibility.Auto
    win.Content <- scroll
    win.Show()

let vectorPlot (txt : TextBox) =
    (fun _ -> 
        let txtXML = txt.Text
        //to do:  put a try with pattern here
        let xml = XElement.Parse txtXML
        let plotObj = plotObject.create xml
        vectorFractal plotObj |> ignore)

//to do:  reference xmlInfo and xmlEquation records
type vectorParameters = {
    name : string; 
    xml : XElement; }

let xname n = XName.op_Implicit (n)
let fetchElement (el : XElement) str = el.Element (xname str)

let populateCbx (cbx : ComboBox) xmlFolder =
    cbx.DisplayMemberPath <- "name"
    let xml = XDocument.Load(xmlFolder + @"\vector.xml")    
    let plots = xml.Element(xname "plots")
    let xelem s el = new XElement(xname s, box el)
    let xatt a b = new XAttribute(xname a, b) |> box
    let xstr s = box s     
    //walk through xml elements and create records        
    for elem in plots.Elements() do
        //to do:  use xmlInfo and xmlEquation records
        let info = elem.Element (xname "info")
        let name = info.Element (xname "name")
        let (param : vectorParameters) = { 
            name = name.Value
            xml = elem; }
        cbx.Items.Add(param) |> ignore
