module PlottingAndScheming.Main

open System
open System.ComponentModel 
open System.Configuration
open System.Data
open System.Data.SqlClient
open System.Diagnostics
open System.IO
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Shapes
open System.Xml.Linq
//open PlottingAndScheming.Scheme
open PlottingAndScheming.Lsystem
open PlottingAndScheming.Vector
open PlottingAndScheming.Complex
open PlottingAndScheming.TwoD
open PlottingAndScheming.Scheme

(* Plotting and Scheming for VS 2012 (.Net 4.5) *)

(* 
to do:

1) fix scheme binding bug
2) xml for everything
3) error tab for scheme
*)

let mainWindow () =
    let win = new Window()
    win.Title <- "Plotting and Scheming 2015"
    let ctl = new UserInterface.Main ()
    //get xml location from App.config
    let xmlFolder = ConfigurationManager.AppSettings.["XmlFolder"]
    //help
    let about = ctl.FindName "mnuAbout" :?> MenuItem
    about.Click.Add
        (fun _ ->
            let winAbout = new Window ()
            winAbout.Title <- "about"
            winAbout.Owner <- win
            winAbout.Width <- 400.0
            winAbout.Height <- 400.0
            winAbout.Content <- new UserInterface.ucAbout ()
            winAbout.Show ())
    let todo = ctl.FindName "mnuTodo" :?> MenuItem
    todo.Click.Add
        (fun _ ->
            let winAbout = new Window ()
            winAbout.Title <- "to do"
            winAbout.Owner <- win
            winAbout.Width <- 400.0
            winAbout.Height <- 400.0
            winAbout.Content <- new UserInterface.ucTodo ()
            winAbout.Show ())    //complex
    let tab = ctl.FindName "tabComplex" :?> TabItem
    let cbx = tab.FindName "cbx" :?> ComboBox
    PlottingAndScheming.Complex.populateCbx cbx xmlFolder |> ignore
    let txt = tab.FindName "txt" :?> TextBox
    let btn = tab.FindName "btn" :?> Button
    btn.Click.Add 
        (fun _ -> 
            let txtXML = txt.Text
            //to do:  put a try with pattern here
            let xml = XElement.Parse txtXML
            let plotObj = plotObject.create xml
            let bm = plotObj.plot ()
            let win = Window()                
            let scroll = ScrollViewer()
            scroll.Content <- plotObj.bitmap.image ()
            scroll.HorizontalScrollBarVisibility <- ScrollBarVisibility.Auto
            scroll.VerticalScrollBarVisibility <- ScrollBarVisibility.Auto
            win.Content <- scroll
            win.SizeToContent <- SizeToContent.WidthAndHeight
            let title = cbx.SelectedValue :?> complexParameters                                   
            win.Title <- title.name
            win.Show() |> ignore
            ())
    cbx.SelectionChanged.Add (
        fun _ -> 
            btn.IsEnabled <- cbx.SelectedIndex <> -1
            let complex = cbx.SelectedValue :?> complexParameters
            txt.Text <- complex.xml.ToString ())
    //l-system
    let tab = ctl.FindName "tabLsystem" :?> TabItem
    let cbx = tab.FindName "cbxLsystem" :?> ComboBox
    PlottingAndScheming.Lsystem.populateCbx cbx xmlFolder |> ignore
    let txt = tab.FindName "txtLsystem" :?> TextBox
    let btn = tab.FindName "btnLsystem" :?> Button
    btn.Click.Add (plot txt)
    cbx.SelectionChanged.Add 
        (fun _ -> 
            btn.IsEnabled <- cbx.SelectedIndex <> -1
            let cbxObj = cbx.SelectedValue :?> comboObject
            txt.Text <- cbxObj.xml.ToString ())
    let txt = tab.FindName "txtLsystemGenerations" :?> TextBox
    let btn = tab.FindName "btnTemp" :?> Button
    btn.Click.Add
        (fun _ ->
            txt.Text <- "hi";
            )
    //vector
    let tab = ctl.FindName "tabVector" :?> TabItem
    let cbx = tab.FindName "cbxVector" :?> ComboBox
    PlottingAndScheming.Vector.populateCbx cbx xmlFolder |> ignore
    let txt = tab.FindName "txtVector" :?> TextBox
    let btn = tab.FindName "btnVector" :?> Button
    btn.Click.Add (vectorPlot txt)
    cbx.SelectionChanged.Add 
        (fun _ -> 
            btn.IsEnabled <- cbx.SelectedIndex <> -1
            let complex = cbx.SelectedValue :?> vectorParameters
            txt.Text <- complex.xml.ToString ())
    //2d
    let tab = ctl.FindName "tab2d" :?> TabItem
    let btn = tab.FindName "btn2d" :?> Button
    let coords = 
        coordinates.originFromUnits
            0.0<units>
            0.0<units>
            20.0<units>           
            500.0<pixels>
            500.0<pixels>
    let worker = new BackgroundWorker()
    worker.DoWork.Add
        (fun args ->
            //let points = walkX coords tan
            let points = walkTheta coords hearts 0.8 45.0 0.01
            let lines = pointsToLines points
            //filter
            //let lines = List.filter (fun x -> checkBounds coords x) lines
            let lines = unitLinesToPixelLines lines coords
            args.Result <- lines)
    worker.RunWorkerCompleted.Add
        (fun args ->
            //let lines = normalize lines coords
            let can = new Canvas()
            can.Width <- float coords.pixelDistanceX
            can.Height <- float coords.pixelDistanceY
            let lines = args.Result :?> line list
            let lines = linesToGuiLines lines
            for el in lines do
                can.Children.Add el |> ignore
            let win = new Window()
            win.Content <- can 
            win.SizeToContent <- SizeToContent.WidthAndHeight
            win.Show() |> ignore            
            btn.IsEnabled <- true)
    btn.Click.Add 
        (fun _ ->
            btn.IsEnabled <- false
            worker.RunWorkerAsync()
            ())
    //3d
    let tab = ctl.FindName "tab3d" :?> TabItem
    let btn = tab.FindName "btn3d" :?> Button
    btn.Click.Add PlottingAndScheming.ThreeD.plot3d
    //scratch
    let tab = ctl.FindName "tabScratch" :?> TabItem
    let btn = tab.FindName "btnScratch" :?> Button

    let innerScope x =
        let x = x + " inner"
        x

    btn.Click.Add
        (fun _ ->
            let foo = innerScope "hey"

            let inFile = @"D:\data\temp\test.txt"
//            let outFile = @"C:\NG-DART\testXml.xml"
            let outFile = @"D:\data\temp\testXml.xml"
            let doc = new XDocument()
            let xn s = XName.Get(s)
            let msgs = new XElement(xn "messages")
            doc.Add(msgs)
            let readLines filePath = System.IO.File.ReadLines(filePath)
            let rec parseStruct (el : XElement) lst = 
                match lst with
                    | [] -> []
                    | h :: t -> 
                        let str : String = h
                        let str = str.Trim ()
                        if str.StartsWith ("//TYPE: ") 
                        then
                            el.SetAttributeValue (xn "type", str.[8..])
                            parseStruct el t
                        elif str.StartsWith ("struct")
                        then
                            let st = new XElement(xn "structure")
                            el.Add (st)
                            st.SetAttributeValue(xn "name", str.[6..].Trim ())
                            parseStruct st t
                        elif str.StartsWith ("end_struct")
                        then t
                        elif str.StartsWith "field"
                        then 
                            let fld = new XElement(xn "field")
                            el.Add fld
                            fld.SetAttributeValue (xn "name", str.[6..].Trim ())
                            parseStruct el t
                        else 
                            parseStruct el t
            let rec parseMsg (el : XElement) lst =
                match lst with
                    | [] -> ignore
                    | h :: t ->
                        let str : String = h
                        let str = str.Trim ()
                        if str.StartsWith ("end_message;")
                        then ignore
                        elif str.StartsWith ("struct")
                        then
                            let st = new XElement(xn "structure")
                            el.Add (st)
                            st.SetAttributeValue(xn "name", str.[6..].Trim ())
                            let rst = parseStruct st t
                            parseMsg el rst
                        elif str.StartsWith ("//TYPE: ")
                        then 
                            parseMsg el t
                        elif str.StartsWith ("field")
                        then 
                            parseMsg el t
                        elif str.StartsWith ("@msgnum")
                        then
                            let str = 
                                Seq.skipWhile (fun el -> el <> '=') str |> 
                                Seq.toArray |> 
                                (fun s -> new System.String(s))
                            let str = str.Replace (';', ' ')
                            el.SetAttributeValue(xn "msgNum", str.[1..].Trim ())
                            parseMsg el t
                        else 
                            parseMsg el t
            let rec parse lst = 
                match lst with
                    | [] -> ignore
                    | h :: t -> 
                        let str : String = h
                        if str.StartsWith (@"//-- message id ")
                        then
                            let el = new XElement(xn "message")
                            msgs.Add(el)
                            el.SetValue (str.[16..])
                            parseMsg el t |> ignore
                            parse t
                        else
                            parse t
            let lst = readLines inFile |> Seq.toList
            let foo = List.ofSeq "abcd" //List.ofSeq "abcd" |> List.toArray |> (fun s -> new System.String(s)) |> printfn "%A"
            parse lst |> ignore
            let txt = doc.ToString()
            doc.Save(outFile) |> ignore )
    //mrcm
    let tab = ctl.FindName "tabMrcm" :?> TabItem
    let btn = tab.FindName "btnMrcm" :?> Button
    btn.Click.Add PlottingAndScheming.MRCM.plotFunc
    //mrcm new
    let tab = ctl.FindName "tabMRCMnew" :?> TabItem
    let btn = tab.FindName "btnMrcmNewScale" :?> Button
    btn.Click.Add
        (fun _ -> 
            let win = new Window ()
            win.Title <- "scale"
            let can  = new Canvas ()
            let pts = 
                [new Point (0.0, 200.0)
                ;new Point (0.0, 0.0)
                ;new Point (200.0, 0.0)
                ;new Point (200.0, 200.0)]
            let mutable mat = Matrix.Identity //new Matrix (1.0, 0.0, 0.0, 1.0, 0.0, 0.0) //identity matrix
            mat.Rotate (90.0)
            mat.Scale(0.5, -0.5)
            mat.OffsetX <- 110.0
            mat.OffsetY <- 400.0
            let ptsArray = List.toArray pts
            mat.Transform (ptsArray)
            let segment = new ResizeArray<Point> ()
            segment.Add ptsArray.[1]
            segment.Add ptsArray.[2]
            let mutable matSegment = Matrix.Identity
            let lastPoint = ptsArray.[3]
            matSegment.OffsetX <- lastPoint.X - ptsArray.[1].X
            matSegment.OffsetY <- lastPoint.Y - ptsArray.[1].Y
            let segArray = segment.ToArray ()
            matSegment.Transform (segArray)
            let foo = new PointCollection ()                        
            for pt in ptsArray do
                foo.Add pt
            foo.Add segArray.[1]
            let poly2 = new Polyline ()
            poly2.Points <- foo
            poly2.Stroke <- new SolidColorBrush(Colors.Black)
            poly2.StrokeThickness <- 2.0
            can.Children.Add poly2 |> ignore           
            win.Content <- can
            win.Show ()            
            )
    let btn = tab.FindName "btnMrcmNewRotate" :?> Button
    btn.Click.Add 
        (fun _ -> 
            ()
            )
    //scheme
    populateEnv ()
    let tab = ctl.FindName "tabScheme" :?> TabItem
    let tabResult = tab.FindName "tabResult" :?> TabItem
    let tabError = tab.FindName "tabError" :?> TabItem
    let txtIn = tab.FindName "txtIn" :?> TextBox
    let txtOut = tabResult.FindName "txtOut" :?> TextBox
    let txtError = tabError.FindName "txtError" :?> TextBox
    let radTokenize = tab.FindName "radTokenize" :?> RadioButton
    let radUntokenize = tab.FindName "radUntokenize" :?> RadioButton
    let radHeapify = tab.FindName "radHeapify" :?> RadioButton
    let radEval = tab.FindName "radEval" :?> RadioButton 
    let btn = tab.FindName "btnGo" :?> Button
    let triToBool (x : Nullable<bool>) =
        if x.HasValue 
        then x.Value
        else false
    btn.Click.Add
        (fun _ -> 
            if triToBool radTokenize.IsChecked then
                let res = parse txtIn.Text
                tabResult.IsSelected <- true
                txtOut.Text <- tokenListToString res
            else if triToBool radUntokenize.IsChecked then
                let res = parse txtIn.Text
                tabResult.IsSelected <- true
                txtOut.Text <- untokenize res
            else if triToBool radHeapify.IsChecked then
                try 
                    let tokens = 
                        parse txtIn.Text |> PlottingAndScheming.Scheme.scmTokens.create
                    tabResult.IsSelected <- true
                    txtOut.Text <- 
                        PlottingAndScheming.Scheme.buildHeap tokens 
                        |> PlottingAndScheming.Scheme.printHeap                    
                with 
                    | ex -> 
                        tabError.IsSelected <- true
                        txtError.Text <- ex.ToString()                        
            else if triToBool radEval.IsChecked then
                let str = txtIn.Text
                try 
                    let evaled = PlottingAndScheming.Scheme.evalString str
                    let dumpHeap = PlottingAndScheming.Scheme.printHeap evaled
                    tabResult.IsSelected <- true
                    txtOut.Text <- dumpHeap
                with 
                    | ex -> 
                        tabError.IsSelected <- true
                        txtError.Text <- ex.ToString()) 
    //hook up user interface control
    win.Content <- ctl
    win

#if COMPILED
[<STAThread()>]
do 
    let win = mainWindow ()
    let app = new Application() in
    app.Run(win) |> ignore
#endif