(* Copyright (c) 2009, 2008, 2007, 2006 by André van Meulebrouck.  All rights reserved worldwide. *)

//new vs. old
module PlottingAndScheming.LsystemOld

open Microsoft.FSharp.Math
open System
open System.Collections.Generic
open System.ComponentModel 
open System.IO
open System.IO.IsolatedStorage
open System.Text.RegularExpressions
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Shapes
open System.Xml.Linq

type rule = {
    lhs : string;
    rhs : string } 

type name = string
type isRule = bool
type isCommand = bool

type LsystemToken = 
    | Primitive of name * isRule * isCommand
    | Multiplier of int

type token = {
    token : string;
    role : string; }
    
type parsedRule = {
    lhs : string; 
    rhs : list<LsystemToken>; }

type LsystemAngle =
    | Degrees of string
    | Directions of string  
    
type lsystemParamaters = { 
    name : string; 
    angle : LsystemAngle; 
    axiom : string;
    rules : seq<rule>;
    len : string;
    scale : string;
    startAngle : string;
    generations : string }

type primitive = {
    name : string; }    
    
type turtle = class
    val mutable strCommand : string
    val mutable len : double
    val mutable xCoordinate : double
    val mutable yCoordinate : double
    val mutable theta : double
    val mutable angle : double
    val mutable toggle : double
    val mutable can : Canvas
    val mutable lines : Stack<Line>
    val mutable loX : double
    val mutable hiX : double
    val mutable loY : double
    val mutable hiY : double
    val mutable repeat : double
    val mutable recursionDepth : double
    val mutable stackAngle : list<double>
    val mutable xStack : Stack<double>
    val mutable yStack : Stack<double>
    val mutable thetaStack : Stack<double>
    val mutable scaleFactor : double
    val mutable startAngle : double
    val mutable clr : SolidColorBrush
    val mutable clrs : SolidColorBrush array
    val mutable levelColor : int
    val mutable depthColor : bool
    val commands : Set<string>
    new () = { 
        strCommand = "";
        len = 15.0; 
        xCoordinate = 0.0;
        yCoordinate = 0.0; 
        theta = 0.0; 
        angle = (Math.PI / 180.0) * 90.0;
        recursionDepth = 0.0;
        can = new Canvas(); 
        lines = new Stack<Line>(); 
        loX = 0.0;
        loY = 0.0;
        hiX = 0.0;
        hiY = 0.0; 
        repeat = 1.0;
        stackAngle = []; 
        xStack = new Stack<double>(); 
        yStack = new Stack<double>(); 
        thetaStack = new Stack<double>(); 
        scaleFactor = 1.0; 
        startAngle = 0.0; 
        clr = new SolidColorBrush(Colors.Blue); 
        toggle = 1.0; 
        clrs = 
            [|  yield new SolidColorBrush(Colors.Blue); 
                yield new SolidColorBrush(Colors.Brown); |]; 
        levelColor = -1; 
        depthColor = false; 
        commands = 
            Set.ofArray 
                [| "f"; "g"; "h"; "+"; "-"; "["; "]"; "|"; "^"; "!"; "C"; "L"; "D"; |]; } 
    member z.x
        with get () = z.xCoordinate
        and set (x : float) = 
            z.xCoordinate <- x         
    member z.y
        with get () = z.xCoordinate
        and set (y : float) = 
            z.yCoordinate <- y    
    member z.go c =
        let computeLength () =
            z.len * Math.Pow(z.scaleFactor, z.depth)           
        match c with 
        | "f" 
        | "h" 
        | "|" -> 
            let (ln : Line) = new Line()
            ln.X1 <- z.xCoordinate
            ln.Y1 <- z.yCoordinate
            let len = computeLength()         
            let x2 = z.xCoordinate + len * Math.Cos z.theta
            let y2 = z.yCoordinate + len * Math.Sin z.theta
            ln.X2 <- x2
            ln.Y2 <- y2
            z.xCoordinate <- x2
            z.yCoordinate <- y2                                       
            let brush = 
                if z.depthColor then
                    let l = Convert.ToInt32(z.recursionDepth)
                    z.clrs.[l % z.clrs.Length]
                else 
                    z.clr
            ln.Stroke <- brush
            ln.StrokeThickness <- 2.0
            //set extrema
            if z.lines.Count = 0 then
                z.loX <- ln.X1
                z.hiX <- ln.X1
                z.loY <- ln.Y1
                z.hiY <- ln.Y1
            else 
                if ln.X1 < z.loX then z.loX <- ln.X1
                if ln.X1 > z.hiX then z.hiX <- ln.X1
                if ln.Y1 < z.loY then z.loY <- ln.Y1
                if ln.Y1 > z.hiY then z.hiY <- ln.Y1
            if ln.X2 < z.loX then z.loX <- ln.X2
            if ln.X2 > z.hiX then z.hiX <- ln.X2
            if ln.Y2 < z.loY then z.loY <- ln.Y2
            if ln.Y2 > z.hiY then z.hiY <- ln.Y2
            //save the line
            z.lines.Push ln
        | "g" -> 
            let len = computeLength()
            z.xCoordinate <- z.xCoordinate + len * Math.Cos z.theta
            z.yCoordinate <- z.yCoordinate + len * Math.Sin z.theta
        | "+" ->
            z.theta <- z.theta - (z.angle * z.repeat * z.toggle)
            if z.repeat <> 1.0 then z.repeat <- 1.0
        | "-" ->
            z.theta <- z.theta + (z.angle * z.repeat * z.toggle)
            if z.repeat <> 1.0 then z.repeat <- 1.0
        | "[" ->
            z.xStack.Push z.xCoordinate
            z.yStack.Push z.yCoordinate
            z.thetaStack.Push z.theta
        | "]" ->
            z.xCoordinate <- z.xStack.Pop()
            z.yCoordinate <- z.yStack.Pop()
            z.theta <- z.thetaStack.Pop()
        | "^" ->
            if z.theta > Math.PI then
                z.theta <- z.theta - Math.PI
            else
                z.theta <- z.theta + Math.PI
        | "!" ->
                z.toggle <- z.toggle * -1.0
        | "C" ->
            let multiplier = Convert.ToInt32(z.repeat)
            z.clr <- z.clrs.[multiplier % z.clrs.Length]
            z.repeat <- 1.0
        | "L" ->
            if z.repeat = z.recursionDepth then
                z.levelColor <- z.levelColor + 1
                z.clr <- z.clrs.[z.levelColor % z.clrs.Length]
            z.repeat <- 1.0
        | "D" ->
            z.depthColor <- true
        | _ -> failwith "unknown command"
    member z.multiplier
        with get () = z.repeat
        and set d = z.repeat <- d
    member z.depth
        with get () = z.recursionDepth
        and set d = z.recursionDepth <- d
    member z.Angle
        with get () = z.angle
        and set d = z.angle <- d
    member z.Length
        with get () = z.len
        and set d = z.len <- d
    member z.Theta
        with get () = z.theta
        and set d = z.theta <- d * (Math.PI / 180.0)
    member z.Scale
        with get () = z.scaleFactor
        and set d = z.scaleFactor <- d
    member z.ColorPalette
        with get() = z.clrs
        and set b = z.clrs <- b
    member z.Color
        with get () = z.clr
        and set b = z.clr <- b
    member z.Commands 
        with get () = z.commands
    member z.canvas 
        with get () =
            //set canvas size
            z.can.Width <- z.hiX - z.loX
            z.can.Height <- z.hiY - z.loY             
            for line in z.lines do
                //normalize lines
                line.X1 <- line.X1 - z.loX
                line.Y1 <- z.hiY - line.Y1
                line.X2 <- line.X2 - z.loX
                line.Y2 <- z.hiY - line.Y2
                //put line into canvas
                z.can.Children.Add line |> ignore
            z.can 
    member z.plot () = 
        MessageBox.Show(z.len.ToString()) |> ignore
    end

let lsystemTabItem () = 
    let tabItem = new TabItem()
    //new vs. old
    tabItem.Name <- "tabLsystemOld"
    //new vs. old
    tabItem.Header <- "l-systemOld"
    let grid = new Grid()
    let col = new ColumnDefinition()
    grid.ColumnDefinitions.Add(col) |> ignore
    let row = new RowDefinition()
    row.Height <- new GridLength(1.0, GridUnitType.Star)        
    grid.RowDefinitions.Add(row) |> ignore
    let row = new RowDefinition()
    row.Height <- new GridLength(155.0, GridUnitType.Star)
    grid.RowDefinitions.Add(row) |> ignore
    let row = new RowDefinition()
    row.Height <- new GridLength(1.0, GridUnitType.Star)
    grid.RowDefinitions.Add(row) |> ignore
    let pnl = new StackPanel()
    let combo = new ComboBox()
    combo.DisplayMemberPath <- "name" 
    grid.Children.Add(combo) |> ignore
    Grid.SetColumn(combo, 0)
    Grid.SetRow(combo, 0)       
    let xml = XDocument.Load("lsystem.xml")        
    let xname n = XName.op_Implicit(n)
    let plots = xml.Element(xname "plots")
    let xelem s el = new XElement(xname s, box el)
    let xatt a b = new XAttribute(xname a, b) |> box
    let xstr s = box s     
    //walk through xml elements and create records        
    for elem in plots.Elements() do
        let name = elem.Element(xname "name")
        let angle = elem.Element(xname "angle")
        let angleMeasure = 
            let mez = angle.Attribute(xname "unitOfMeasure")
            let angleValue = angle.Value.ToString()
            let measureAtt = 
                if mez = null then ""
                else mez.Value.ToString()
            if measureAtt = "" || measureAtt = "degrees" then
                LsystemAngle.Degrees(angleValue)
            elif measureAtt = "directions" then
                LsystemAngle.Directions(angleValue)
            else failwith "unknown angle attribute %A" measureAtt
        let axiom = elem.Element(xname "axiom")
        let parameters = elem.Element(xname "parameters")            
        let generations = 
            if parameters <> null then 
                parameters.Element(xname "generations")
            else null
        let vector = 
            if parameters <> null then
                parameters.Element(xname "vector")
            else null
        let vecLen =
            if vector <> null then
                vector.Element(xname "length")
            else null
        let vecAngle =
            if vector <> null then
                vector.Element(xname "angle")
            else null
        let scaleFactor =
            if vector <> null then
                vector.Element(xname "scale")
            else null
        let rules = elem.Element(xname "rules")                          
        let param = { 
            name = name.Value.ToString(); 
            angle = angleMeasure;
            axiom = axiom.Value.ToString();
            len = 
                if vecLen <> null then 
                    vecLen.Value.ToString()
                else "";
            scale = 
                if scaleFactor <> null then
                    scaleFactor.Value.ToString()
                else
                    "1.0"
            startAngle = 
                if vecAngle <> null then
                    vecAngle.Value.ToString()
                else ""
            rules = 
                rules.Elements() |> 
                Seq.map 
                    (fun e -> 
                        let lhs = e.Element(xname "lhs")
                        let rhs = e.Element(xname "rhs")
                        {   lhs = lhs.Value.ToString(); 
                            rhs = rhs.Value.ToString() });
            generations = 
                if generations <> null then 
                    generations.Value.ToString()
                else "1" }
        combo.Items.Add(param) |> ignore            
    let txt = new TextBox()
    txt.AcceptsReturn <- true
    txt.AcceptsTab <- true
    grid.Children.Add(txt) |> ignore
    Grid.SetColumn(txt, 0)
    Grid.SetRow(txt, 1)              
    let btn = new Button()
    btn.IsEnabled <- false
    btn.Content <- "plot"
    //event handler for plot button
    btn.Click.Add 
        (fun _ -> 
            let tur = new turtle()
            let lsystem = combo.SelectedValue :?> lsystemParamaters
            let aryRules =
                lsystem.rules |> 
                Seq.map (fun e -> e.lhs) |> 
                Seq.toList
            //function to parse RHS
            let parseRHS (x : string) = 
                let setRules = Set.ofSeq aryRules
                let setPrimitives = tur.Commands                    
                let rec recur (i : int, l : list<LsystemToken>) =
                    if i = x.Length then
                        List.rev l
                    else
                        let c = x.Substring(i, 1)  
                        if c >= "0" && c <= "9" then
                            let rec parseMultiplier max =
                                if max = x.Length then
                                    max - 1
                                else 
                                    let c = x.Substring(max, 1)
                                    if c >= "0" && c <= "9" then
                                        parseMultiplier (max + 1)
                                    else
                                        max - 1
                            let max = parseMultiplier(i + 1)
                            recur (max + 1, LsystemToken.Multiplier(Int32.Parse(x.Substring(i, max - i + 1))) :: l)
                        else
                            let isRule = setRules.Contains c                                
                            let isCommand = setPrimitives.Contains c
                            if isRule || isCommand then
                                recur (i + 1, LsystemToken.Primitive(c, isRule, isCommand) :: l)
                            elif c = " " then
                                recur (i + 1, l)
                            else 
                                failwith "unknown token %A" c
                recur (0, [])                                                             
            //walk through RHS of each rule and parse into tokens
            let aryRuleValues = 
                lsystem.rules |> 
                Seq.map (fun e -> parseRHS e.rhs) |> 
                Seq.toList
            let axiom = lsystem.axiom |> parseRHS
            let getRule r = 
                let rec recur lhs rhs =
                    match lhs with 
                    | [] -> failwith "rule not found"
                    | h :: t -> 
                        if h = r then List.head rhs
                        else recur t (List.tail rhs)
                recur aryRules aryRuleValues
            let n = ref -1
            let traverse axiom lhs rhs generations (tur : turtle) = 
                let max = generations - 1
                let rec recur level generation = 
                    if level = max then
                        match generation with
                        | [] -> ()
                        | h :: t -> 
                            match h with
                            | LsystemToken.Primitive(p, r, c) ->
                                if c then
                                    tur.depth <- Convert.ToDouble(level)
                                    tur.go p
                                    recur level t
                                else
                                    recur level t
                            | LsystemToken.Multiplier(n) -> 
                                tur.multiplier <- Convert.ToDouble n
                                recur level t
                    else
                        match generation with 
                        | [] -> ()
                        | h :: t ->
                            match h with 
                            | LsystemToken.Primitive(p, r, c) ->
                                if r then
                                    //colorizer tur level
                                    recur (level + 1) (getRule p)
                                    recur level t
                                else 
                                    tur.depth <- Convert.ToDouble(level)
                                    tur.go p
                                    recur level t 
                            | LsystemToken.Multiplier(n) -> 
                                tur.multiplier <- Convert.ToDouble n
                                recur level t
                recur 0 axiom |> ignore                
            tur.ColorPalette <- 
                [|  yield new SolidColorBrush(Colors.Blue);
                    yield new SolidColorBrush(Colors.Green);
                    yield new SolidColorBrush(Colors.Gold);
                    yield new SolidColorBrush(Colors.Purple);
                    yield new SolidColorBrush(Colors.Coral); 
                    yield new SolidColorBrush(Colors.Brown);
                    yield new SolidColorBrush(Colors.Aquamarine);
                    yield new SolidColorBrush(Colors.Turquoise);
                    yield new SolidColorBrush(Colors.Silver);
                    yield new SolidColorBrush(Colors.Maroon); 
                    yield new SolidColorBrush(Colors.Orange); |]
            tur.Angle <- 
                match lsystem.angle with 
                | LsystemAngle.Degrees(s) -> (Double.Parse s) * (Math.PI / 180.0)
                | LsystemAngle.Directions(s) -> 2.0 * Math.PI / (Double.Parse s)
            let len = 
                if lsystem.len = "" then 0.0
                else Double.Parse lsystem.len
            let getValForKeyword keyword txt =
                let keywordRegex = new Regex(keyword)
                let delimiter = new Regex("\n")
                let start = keywordRegex.Match(txt)
                let hit = txt.Substring(start.Index + keyword.Length)
                let fin = delimiter.Match(hit)
                let (res : string) = 
                    if fin.Index = 0 then hit.Substring(0) else hit.Substring(0, fin.Index)
                let finalResult = res.ToString().Trim()
                finalResult
            let newLen = getValForKeyword "length:" txt.Text
            let intLen = Int32.Parse newLen
            let dblLen = Convert.ToDouble intLen
            tur.Length <- dblLen
            tur.scaleFactor <- Double.Parse lsystem.scale
            let startAngle = lsystem.startAngle
            let startAngle = 
                if startAngle = "" then 0.0
                else (Double.Parse lsystem.startAngle)
            tur.Theta <- startAngle
            let generations = (Int32.Parse lsystem.generations)
            let temp = getValForKeyword "generations:" txt.Text
            let generations = Int32.Parse temp
            let foo = 
                traverse 
                    axiom                                               
                    aryRules
                    aryRuleValues
                    generations
                    tur                                
            let win = new Window()
            let can = tur.canvas
            let scroll = ScrollViewer()
            scroll.Content <- can
            scroll.HorizontalScrollBarVisibility <- ScrollBarVisibility.Auto
            scroll.VerticalScrollBarVisibility <- ScrollBarVisibility.Auto
            win.Content <- scroll
            win.Title <- lsystem.name + " (generation " + generations.ToString() + ")"
            win.Show() |> ignore
            )
    //event handler for combo select
    combo.SelectionChanged.Add
        (fun _ -> 
            btn.IsEnabled <- combo.SelectedIndex <> -1
            let lsystem = combo.SelectedValue :?> lsystemParamaters
            let strRules = 
                lsystem.rules |> 
                Seq.map (fun e -> e.lhs + " = " + e.rhs + "\n") |> 
                Seq.toArray |>
                String.concat System.String.Empty
            let angle =
                match lsystem.angle with
                | LsystemAngle.Degrees(s) -> s + " degrees"
                | LsystemAngle.Directions(s) -> s + " directions"
            let str = 
                "name:  " + lsystem.name.ToString() + "\n" +
                "angle:  " + angle + "\n" + 
                "axiom = " + lsystem.axiom.ToString() + "\n" + 
                strRules + 
                "generations:  " + lsystem.generations.ToString() +  "\n" +
                "length:  " + lsystem.len +
                if lsystem.scale <> "1.0" then
                    "\n" + "scale factor:  " + lsystem.scale 
                else ""
            txt.Text <- str
            )                    
    grid.Children.Add(btn) |> ignore        
    Grid.SetColumn(btn, 0)
    Grid.SetRow(btn, 2)
    pnl.Children.Add(grid) |> ignore
    tabItem.Content <- pnl
    tabItem
