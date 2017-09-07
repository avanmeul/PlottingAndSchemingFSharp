(* Copyright (c) 2009, 2008, 2007, 2006 by André van Meulebrouck.  All rights reserved worldwide. *)

module PlottingAndScheming.Complex

open Microsoft.FSharp.Math
open PlottingAndScheming.TwoD
open System
open System.ComponentModel 
open System.IO
open System.Collections.Generic
open System.IO.IsolatedStorage
open System.Numerics
open System.Text.RegularExpressions
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Shapes
open System.Xml.Linq

//the goal of this module is to do complex plane fractals in an OOP style

type 'a options =
    | Empty 
    | Contents of 'a
    | Error of string

type iteratedSet = 
    | Mandelbrot of complex Option
    | Julia of complex
    | NewtonBOF of float

type fateOrbitNumbering = 
    | Down
    | Up

type iteratedFateAlgorithm =
    | Remainder 
    | Quotient

type orbitPattern =
    | Fixpoint
    | Oscillate

//equations

type newtonEquationBOF =
    {   f : (complex -> complex);
        f' : (complex -> complex); }

type newtonEquation =
    {   f : (complex -> complex -> complex); 
        f' : (complex -> complex -> complex); }

type complexEquation = 
    | Generic of (complex -> complex -> complex)
    | Phoenix of (complex -> complex -> complex -> complex)
    | NewtonBOF of newtonEquationBOF
    | Newton of newtonEquation

type equation = {
    name : string;
    equation : complexEquation; }

let complexOne = complex 1.0 0.0
let complexTwo = complex 2.0 0.0
let complexThree = complex 3.0 0.0
let complexI = complex 0.0 1.0
let complexZero = complex 0.0 0.0

let (equations : equation list) = [
    {   name = "mandelbrot"; 
        equation = 
            complexEquation.Generic
                (fun c z ->
                    (pown z 2) + c); };
    {   name = "San Marco dragon";
        equation = 
            complexEquation.Generic 
                (fun c z ->
                    c * (pown (complexOne - z) 2)); };
    {   name = "z cubed";
        equation = 
            complexEquation.Generic
                (fun c z -> 
                    (pown z 3) + c); };
    {   name = "cos";
        equation = 
            complexEquation.Generic
                (fun c z -> 
                    (cos z) + c); };
    {   name = "phoenix";
        equation = 
            complexEquation.Phoenix
                (fun c z p ->
                    z * z +
                    (complex c.r 0.0) +
                    (c.i * p)); };
    {   name = "potts-spins model i";
        equation = 
            complexEquation.Generic
                (fun c z -> 
                    (pown (((z * z) + c - complexOne) / (2.0 * z + c - complexTwo)) 2)); }
    {   name = "potts-spins model ii";
        equation = 
            complexEquation.Generic
                (fun c z -> 
                    let cmpCminus1 = c - complexOne
                    let cmpCminus2 = c - complexTwo
                    let cmpCminus1timesCminus2 = cmpCminus1 * cmpCminus2
                    let cmpCsqMinus3cPlus3 = c * c - (complexThree * c) + complexThree
                    let cmpZsq = z * z
                    let newZ = 
                        ((cmpZsq * z) + 
                            (complexThree * cmpCminus1 * z) + cmpCminus1timesCminus2) /
                        (complexThree * cmpZsq + 
                            (complexThree * cmpCminus2 * z) + cmpCsqMinus3cPlus3)
                    newZ * newZ); };
    {   name = "critter";
        equation = 
            complexEquation.NewtonBOF {   
                f = 
                    (fun z ->
                        (z - complexOne) * (z * z + z + (complex 0.5 0.0)));
                f' = 
                    (fun z ->
                        ((z - complexOne) * ((complexTwo * z) + complexOne)) + 
                        (z * z) + z + (complex 0.5 0.0)); }; };
    {   name = "plate 27";
        equation = 
            complexEquation.Newton {
                f = 
                    (fun c z -> (pown z 3) - c);
                f' = 
                    (fun c z -> 3.0 * (pown z 2)); }; };
    {   name = "plate 28";
        equation = 
            complexEquation.Newton {
                f = 
                    (fun c z -> (pown z 3) - (2.0 * z) + c);
                f' =
                    (fun c z -> 3.0 * (pown z 2) - complexTwo); }; };
    {   name = "z^2";
        equation = 
            complexEquation.Newton {
                f = 
                    (fun c z -> (pown z 2) + c);
                f' = 
                    (fun c z -> 2.0 * z); }; };
    {   name = "z^3";
        equation = 
            complexEquation.Newton {
                f = 
                    (fun c z -> (pown z 3) + c);
                f' = 
                    (fun c z -> 3.0 * (pown z 2)); }; };
    {   name = "z^4";
        equation = 
            complexEquation.Newton {
                f = 
                    (fun c z -> (pown z 4) + c);
                f' = 
                    (fun c z -> 4.0 * (pown z 3)); }; };
    {   name = "z^5";
        equation = 
            complexEquation.Newton {
                f = 
                    (fun c z -> (pown z 5) + c);
                f' = 
                    (fun c z -> 5.0 * (pown z 4)); }; }; 
    {   name = "z^6";
        equation = 
            complexEquation.Newton {
                f = 
                    (fun c z -> (pown z 6) + c);
                f' = 
                    (fun c z -> 6.0 * (pown z 5)); }; }; 
    {   name = "z^7";
        equation = 
            complexEquation.Newton {
                f = 
                    (fun c z -> (pown z 7) + c);
                f' = 
                    (fun c z -> 7.0 * (pown z 6)); }; }; 
    ]

let equationFind name =
    let rec iter lst =
        match lst with
        | [] -> None
        | h :: t -> 
            if h.name = name then
                Some h.equation
            else 
                iter t
    iter equations

type iteratedFateColoring = {
    palette : Color array;
    orbitNumbering : fateOrbitNumbering;
    algorithm : iteratedFateAlgorithm; } with
    member x.getColor (iterations : int) (iterationsMax : int) =
        let iterationsMax = iterationsMax - 1
        let len = x.palette.Length
        let i = 
            match x.orbitNumbering with
            | fateOrbitNumbering.Up -> iterations
            | fateOrbitNumbering.Down -> (iterationsMax - iterations)
        match x.algorithm with
        | iteratedFateAlgorithm.Quotient -> 
            //to do:  get rid of hardcoded value, make union with len or #
            let numberOfGroups = 35
            x.palette.[(i / numberOfGroups) % len]
        | iteratedFateAlgorithm.Remainder -> 
            x.palette.[i % len]

let schemeParseComplex c =
    let pat = "([+|-]{0,1}[0-9]*[.]*[0-9]+)([+|-]{1}[0-9]*[.]*[0-9]*)[i]"
    if Regex.IsMatch (c, pat) then
        let r = Regex pat
        let s = r.Match c
        let n = s.Groups
        if n.Count = 3 then
            let real = n.[1]
            let imag = n.[2]
            let s, v = Double.TryParse real.Value
            let real = if s then Some v else None
            let s, v = Double.TryParse imag.Value
            let imag = if s then Some v else None
            if real.IsSome && imag.IsSome then
                Some (complex real.Value imag.Value)
            else 
                None
        else None
    else
        None

type iteratedSurvivor = {
    iterations : int; //to do:  remove this
    coloring : iteratedFateColoring; } with    
    member x.checkFate (iterations : int) (iterationsMax : int) =
        if iterations >= iterationsMax - 1 then
            Some (x.coloring.getColor iterations iterationsMax)
        else
            None

type iteratedEscapee = {
    threshold : double;
    coloringInfinity : iteratedFateColoring;
    coloringBiomorph : iteratedFateColoring option; } with 
    member x.checkFate (z : complex) (iterations : int) (iterationsMax : int) =
        let outOfBounds = z.Magnitude > x.threshold
        match x.coloringBiomorph, outOfBounds with
        | Some b, true ->
            let i = z.i
            let r = z.r
            if i * i <= x.threshold || r * r <= x.threshold then
                Some (b.getColor iterations iterationsMax)
            else 
                Some (x.coloringInfinity.getColor iterations iterationsMax)
        | None, true -> 
            Some (x.coloringInfinity.getColor iterations iterationsMax)
        | _, false -> 
            None

type iteratedFixpoint = {
    fixpoint : complex;
    epsilon : complex option;
    coloring : iteratedFateColoring; } with
    member x.checkFate (z : complex) (iterations : int) (iterationsMax : int) =        
        if z = x.fixpoint then
            Some (x.coloring.getColor iterations iterationsMax)
        else 
            None
    
type iteratedCriticalValue = {
    criticalValue : complex;
    iteratedCriticalValue: complex;
    comparison : float -> float -> bool;
    epsilon : float option;
    coloring : iteratedFateColoring; } with
    member x.checkFate (z : complex) (iterations : int) (iterationsMax : int) = 
        match x.epsilon with
        | None ->       
            if z = x.iteratedCriticalValue then
                Some (x.coloring.getColor iterations iterationsMax)
            else 
                None
        | Some e ->
            if x.comparison (z - x.iteratedCriticalValue).Magnitude e then
                Some (x.coloring.getColor iterations iterationsMax)
            else 
                None

let xname n = XName.op_Implicit(n)

//to do:  make a union with none, value, and parse error
let fetch tryParse (xml : XElement) name =
    let x = xml.Element (xname name)       
    match x with 
    | null -> None
    | _ -> 
        let s, v = tryParse (x.Value)
        if s then 
            Some v
        else 
            failwith (name + " parse failure in " + (x.ToString ()))

let fetchDouble = fetch Double.TryParse
//let fetchBoolean = fetch Boolean.TryParse
let fetchInt : XElement -> string -> Int32 option = fetch Int32.TryParse
let fetchElement (el : XElement) str = el.Element (xname str)
let fetchAttribute (el : XElement) str = 
    let at = el.Attribute (xname str)
    if at = null then 
        None
    else
        Some at.Value

type plotCoordinates = {
    xml : XElement;
    swapXY : bool option;
    coords : coordinates; } with
    static member create (xml : XElement) = 
        let swapXY = 
            Option.bind 
                (fun v -> 
                    let b, v = Boolean.TryParse v
                    if b then Some v else None)                
                (fetchAttribute xml "swapXY")
        let x = xml.Element (xname "x")
        let pixelsX = fetchDouble x "pixels"
        let pixelsX = 
            match pixelsX with
            | Some x -> Some (x - 1.0)
            | None -> None
        let centerX = fetchDouble x "center"
        let minX = if centerX.IsNone then fetchDouble x "min" else None
        let maxX = if centerX.IsNone then fetchDouble x "max" else None
        let y = xml.Element (xname "y")
        let centerY = fetchDouble y "center"
        let minY = if centerY.IsNone then fetchDouble y "min" else None
        let maxY = if centerY.IsNone then fetchDouble y "max" else None
        let pixelsY = fetchDouble y "pixels"
        let pixelsY = 
            match pixelsY with
            | Some y -> Some (y - 1.0)
            | None -> None
        let ppu = fetchDouble xml "ppu"
        //call coordinate constructor
        let coords = 
            match centerX, centerY with 
            | Some x, Some y ->
                let centerX = x * 1.0<units>
                let centerY = y * 1.0<units>
                let ppu = ppu.Value * 1.0<pixels/units>
                let pixelsX = pixelsX.Value * 1.0<pixels>
                let pixelsY = pixelsY.Value * 1.0<pixels>
                let coords =
                    coordinates.originFromPPU
                        centerX
                        centerY
                        ppu
                        pixelsX
                        pixelsY
                coords
            | None, None ->
                match minX, minY, maxX, maxY with 
                | Some minX, Some minY, Some maxX, Some maxY -> 
                    let minX = minX * 1.0<units>
                    let maxX = maxX * 1.0<units>
                    let minY = minY * 1.0<units>
                    let maxY = maxY * 1.0<units>
                    let pixelsX = pixelsX.Value * 1.0<pixels>
                    let pixelsY = pixelsY.Value * 1.0<pixels>
                    let coords =
                        coordinates.extremas
                            minX
                            maxX
                            minY
                            maxY
                            pixelsX
                            pixelsY
                    coords
                | _, _, _, _ -> failwith "bad coordinate specification for extremas"
            | _, _ -> failwith "unexpected coordinate specifications"
        {   xml = xml;
            swapXY = swapXY;
            coords = coords; }

type plotPalettes = {
    xml : XElement;
    palettes : Color array array; } with 
    static member create (xml : XElement) = 
        let colors = 
            xml.Elements ()
            |> Seq.map 
                (fun e -> 
                    e.Elements () 
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
                    |> Seq.toArray)
            |> Seq.toArray
        {   xml = xml;
            palettes = colors; }
    member x.palette n =
        x.palettes.[n]

type xmlColoring = {
    xml : XElement;
    coloring : iteratedFateColoring; } with
    static member fromXML (xml : XElement) (palettes : plotPalettes) = 
        let palette = fetchInt xml "palette"
        let algorithm = xml.Element (xname "algorithm")
        let algorithm = 
            match algorithm.Value with 
            | "remainder" -> iteratedFateAlgorithm.Remainder
            | "quotient" -> iteratedFateAlgorithm.Quotient
            | x -> failwith ("bad orbit numbering specification" + x)
        let orbitNumbering = xml.Element (xname "orbitNumbering")
        let orbitNumbering = 
            match orbitNumbering.Value with 
            | "up" -> fateOrbitNumbering.Up
            | "down" -> fateOrbitNumbering.Down
            | x -> failwith ("bad orbit numbering specification" + x)
        let palette = palettes.palette palette.Value
        let coloring = 
            {   palette = palette;
                algorithm = algorithm;
                orbitNumbering = orbitNumbering; }
        {   xml = xml;
            coloring = coloring; }

type xmlFateSurvivor = {
    xml : XElement;
    survivor : iteratedSurvivor; } with
    static member fromXML (xml : XElement) (palettes : plotPalettes) = 
        let iterations = fetchInt xml "iterations"
        let coloring = fetchElement xml "coloring"
        let coloring = xmlColoring.fromXML coloring palettes
        let survivor = {
            iterations = iterations.Value;
            coloring = coloring.coloring; }
        {   xml = xml;
            survivor = survivor; }

type xmlFateEscapee = {
    xml : XElement;
    escapee : iteratedEscapee; } with
    static member fromXML (xml : XElement) (palettes : plotPalettes) = 
        let threshold = fetchDouble xml "threshold" 
        let infinity = fetchElement xml "infinity"
        let biomorph = fetchElement xml "biomorph"
        let coloringInfinity = fetchElement infinity "coloring"
        let coloringInfinity = xmlColoring.fromXML coloringInfinity palettes
        let coloringInfinity = coloringInfinity.coloring
        let coloringBiomorph = 
            if biomorph = null then
                None
            else
                let coloringBiomorph = fetchElement biomorph "coloring"
                let coloringBiomorph = xmlColoring.fromXML coloringBiomorph palettes
                Some coloringBiomorph.coloring
        let escapee = {
            threshold = threshold.Value;
            coloringInfinity = coloringInfinity; 
            coloringBiomorph = coloringBiomorph;  }
        {   xml = xml;
            escapee = escapee; }

type xmlFateFixpoint = {
    xml : XElement;
    fixpoint : iteratedFixpoint; } with
    static member fromXML (xml : XElement) (palettes : plotPalettes) =
        let f = fetchElement xml "value"
        let f = 
            if f = null then 
                None
            else 
                schemeParseComplex f.Value
        if f.IsNone then failwith "bad fixpoint"
        let coloring = fetchElement xml "coloring"
        let coloring = xmlColoring.fromXML coloring palettes
        let coloring = coloring.coloring
        let fixpoint =
            {   fixpoint = f.Value; 
                epsilon = None;
                coloring = coloring; }
        {   xml = xml;
            fixpoint = fixpoint; }         

type iteratorMandelbrot = {
    mutable z : complex option;
    mutable c : complex option;
    mutable iterations : int option;
    set : iteratedSet;
    equation : complex -> complex -> complex; } with
    static member create eq set =
        match set with
        | iteratedSet.Mandelbrot o -> 
            {z = None; c = None; iterations = None; set = set; equation = eq; }
        | iteratedSet.Julia o -> 
            {z = None; c = Some o; iterations = None; set = set; equation = eq; }
        | _ -> 
            failwith "not implemented yet"
    member x.init z =
        x.iterations <- Some 0
        match x.set with 
        | iteratedSet.Mandelbrot _ ->
            x.z <- Some (complex 0.0 0.0)
            x.c <- Some z
            ()
        | iteratedSet.Julia _ ->
            x.z <- Some z
            ()
        | _ -> failwith "not implemented yet"
    member x.next () =
        match x.z, x.c with 
        | Some z, Some c ->
            x.z <- Some (x.equation c z)
        | _, _ ->
            failwith "attempt to iterate before initialization"
        match x.iterations with
        | Some i ->
            x.iterations <- Some (i + 1)
        | None ->
            failwith "attempt to iterate before initialization"
        ()

type iteratorPhoenix = {
    mutable p : complex option;
    mutable z : complex option;
    mutable c : complex option;
    mutable iterations : int option;
    set : iteratedSet;
    equation : complex -> complex -> complex -> complex; } with
    static member create eq set =
        match set with
        | iteratedSet.Mandelbrot o -> 
            {p = None; z = None; c = None; iterations = None; set = set; equation = eq; }
        | iteratedSet.Julia o -> 
            {p = None; z = None; c = Some o; iterations = None; set = set; equation = eq; }
        | _ -> 
            failwith "not implemented yet"
    member x.init z =
        x.iterations <- Some 0
        x.p <- Some complexZero
        match x.set with 
        | iteratedSet.Mandelbrot _ ->
            x.z <- Some complexZero
            x.c <- Some z
            ()
        | iteratedSet.Julia _ ->
            x.z <- Some z
            ()
        | _ -> failwith "illegal set for phoenix iterator"
    member x.next () =
        match x.z, x.c, x.p with 
        | Some z, Some c, Some p ->
            let previous = z
            let nextZ = x.equation c z p
            x.z <- Some nextZ
            x.p <- Some previous
        | _, _, _ ->
            failwith "attempt to iterate before initialization"
        match x.iterations with
        | Some i ->
            x.iterations <- Some (i + 1)
        | None ->
            failwith "attempt to iterate before initialization"
        ()

type iteratorNewtonBOF = {
    mutable z : complex option;
    mutable iterations : int option;
    f : complex -> complex; 
    f' : complex -> complex; 
    s : complex; } with 
    static member create (eq : newtonEquationBOF) s =
        {z = None; iterations = None; f = eq.f; f' = eq.f'; s = s}
    member x.init z = 
        x.z <- Some z
        x.iterations <- Some 0
        ()
    member x.next () = 
        match x.z, x.s with
        | Some z, s ->

            x.z <- Some (z - ((x.f z) / ((x.f' z) + (s * complexI))))
        | _, _ -> 
            failwith "attempt to iterate before initialization"
        match x.iterations with
        | Some i -> 
            x.iterations <- Some (i + 1)
        | None -> 
            failwith "attempt to iterate before initilization"
        ()

type iteratorNewton = {
    mutable z : complex option;
    mutable iterations : int option;
    mutable orbits : complex list;
    f : complex -> complex -> complex; 
    f' : complex -> complex -> complex; 
    mutable c : complex option; 
    set : iteratedSet;} with 
    static member create (eq : newtonEquation) set =
        let c =
            match set with 
            | iteratedSet.Julia j ->
                Some j
            | iteratedSet.Mandelbrot m ->
                m
            | iteratedSet.NewtonBOF s -> 
                failwith "inappropriate set argument for this type of newton"
        {z = None; iterations = None; f = eq.f; f' = eq.f'; set = set; c = c; orbits = []}
    member x.init z = 
        x.z <- Some z
        x.iterations <- Some 0
        match x.set with 
        | iteratedSet.Mandelbrot _ ->
            x.z <- Some complexZero
            x.c <- Some z
            x.orbits <- []
            ()
        | iteratedSet.Julia _ ->
            x.orbits <- []
            x.z <- Some z
            ()
        | _ -> failwith "illegal set for this type of newton iterator"
        ()
    member x.next () = 
        match x.z, x.c with
        | Some z, Some c ->
            x.orbits <- z :: x.orbits  
            x.z <- Some (z - ((x.f c z) / ((x.f' c z))))
        | _, _ -> 
            failwith "attempt to iterate before initialization"
        match x.iterations with
        | Some i -> 
            x.iterations <- Some (i + 1)
        | None -> 
            failwith "attempt to iterate before initilization"
        ()

type iterator =
    | Mandelbrot of iteratorMandelbrot
    | Phoenix of iteratorPhoenix 
    | Newton of iteratorNewton
    | NewtonBOF of iteratorNewtonBOF

//master to do list
//priority #1
//to do:  newton
//to do:  abs(magnitude(z) - magnitude(cv)) < epsilon
//to do:  interior coloring

type xmlIterator = {
    xml : XElement;
    iterator : iterator; } with
    static member create (xml : XElement) =
        let it = fetchElement xml "iterator"
        if it <> null then
            let set = fetchElement it "set"
            let setName = set.Attribute (xname "type")
            let set = 
                match setName with
                | null ->
                    failwith "set type not specified"
                | a -> 
                    match a.Value with 
                    | "mandelbrot" -> 
                        iteratedSet.Mandelbrot None
                    | "julia" -> 
                        let c = set.Attribute (xname "constant")
                        match c with
                        | null -> failwith "no constant specified"
                        | _ -> 
                            let c = schemeParseComplex c.Value
                            match c with
                            | Some c ->
                                iteratedSet.Julia c
                            | None ->
                                failwith "incorrectly formatted Julia constant"
                    | _ -> 
                        //to do:  don't hardcode
                        iteratedSet.NewtonBOF 0.0
            let eq = fetchElement it "function"
            let itType = it.Attribute (xname "type")
            let fname = 
                match eq with 
                | null -> failwith "could not find function specification for iterator"
                | _ -> 
                    let fname = eq.Attribute (xname "name")
                    fname.Value
            let eq = equationFind fname
            let eq = 
                if eq.IsNone then
                    failwith "bad equation"
                else
                    eq.Value
            let it = 
                match itType with
                | null -> failwith "unspecified iterator"
                | _ ->
                    match itType.Value, eq with
                    | "generic", complexEquation.Generic g ->
                        iterator.Mandelbrot (iteratorMandelbrot.create g set)
                    | "phoenix", complexEquation.Phoenix p -> 
                        iterator.Phoenix (iteratorPhoenix.create p set)
                    | "newton BOF", complexEquation.NewtonBOF n ->
                        iterator.NewtonBOF (iteratorNewtonBOF.create n complexZero)
                    | "newton", complexEquation.Newton n ->
                        iterator.Newton (iteratorNewton.create n set)
                    | _, _ -> failwith "unknown iterator type"               
            {xml = xml; iterator = it}
        else 
            failwith "bad iterator specified"

type xmlFateCriticalValue = {
    xml : XElement;
    criticalValue : iteratedCriticalValue; } with
    static member fromXML 
        (xml : XElement) (palettes : plotPalettes) (it : iterator) (iterationMax : int) =
        //critical value
        let cv = fetchElement xml "value"
        let cv =
            match cv with
            | null -> failwith "no critical value specified"
            | _ -> 
                let cv = schemeParseComplex cv.Value
                match cv with
                | Some cv ->
                    cv
                | None ->
                    failwith "incorrectly formatted complex number"
        //iterate the critical value
        let iteratedCV =
            match it with
            | iterator.Mandelbrot m ->
                m.init cv
                //iterate this iteration times
                for i in [0 .. iterationMax - 1] do
                    m.next ()
                m.z
            | iterator.Phoenix p -> 
                p.init cv
                //iterate this iteration times
                for i in [0 .. iterationMax - 1] do
                    p.next ()
                p.z
            | iterator.NewtonBOF n ->
                n.init cv
                for i in [0 .. iterationMax - 1] do
                    n.next ()
                n.z
            | iterator.Newton n ->
                n.init cv
                for i in [0 .. iterationMax - 1] do
                    n.next ()
                n.z
        let coloring = fetchElement xml "coloring"
        let coloring = xmlColoring.fromXML coloring palettes
        let coloring = coloring.coloring
        let value =
            {   criticalValue = cv; 
                iteratedCriticalValue = iteratedCV.Value;
                comparison = (<);
                epsilon = Some 0.0003;
                coloring = coloring; }
        {   xml = xml; criticalValue = value; }

type iteratedOrbitCheck = {
    orbitType : orbitPattern;
    mutable matches : complex list; //to do:  add matches to this list
    coloring : iteratedFateColoring; } with
    member x.checkFate (it : iterator) (iterationsMax : int) =
        let iterations, z, orbits = 
            match it with
            | Newton n -> 
                n.iterations, n.z, n.orbits
            | _ -> 
                failwith "can't check orbits on the given iterator"
        match x.orbitType with
        | orbitPattern.Fixpoint -> 
            if orbits.Length >= 2 && z.IsSome && iterations.IsSome then
                let t = orbits |> List.tail |> List.head
                if z.Value = t then
                    Some (x.coloring.getColor iterations.Value iterationsMax)
                else 
                    None
            else 
                None
        | orbitPattern.Oscillate ->
            if orbits.Length >= 3 && z.IsSome && iterations.IsSome then
                let t = orbits |> List.tail |> List.tail |> List.tryFind (fun x -> x = z.Value)
                if t.IsSome then
                    //to do:  add to matches
                    Some (x.coloring.getColor iterations.Value iterationsMax)
                else 
                    None
            else 
                None

type xmlFateOrbitCheck = {
    xml : XElement;
    orbitChecker : iteratedOrbitCheck; } with
    static member fromXML 
        (xml : XElement) (palettes : plotPalettes) (it : iterator) =
        let coloring = fetchElement xml "coloring"
        let coloring = xmlColoring.fromXML coloring palettes
        let coloring = coloring.coloring
        let o = fetchElement xml "value"
        let orbitCheckType =
            if o <> null then
                match o.Value with
                | "fixpoint" ->
                    Some orbitPattern.Fixpoint
                | "oscillation" -> 
                    Some orbitPattern.Oscillate
                | _ -> None
            else
                None
        let oc =
            if orbitCheckType.IsSome then
                { orbitType = orbitCheckType.Value; matches = []; coloring = coloring }
            else
                failwith "bad information for orbit check"
        { xml = xml; orbitChecker = oc }

type fate =
    | Survivor of xmlFateSurvivor
    | Escapee of xmlFateEscapee
    | Fixpoint of xmlFateFixpoint
    | CriticalValue of xmlFateCriticalValue
    | OrbitCheck of xmlFateOrbitCheck

type plotFates = {
    xml : XElement;
    iterationMax : int;
    fates : fate list;
    } with
    static member create (xml : XElement) (palettes : plotPalettes) (it : iterator) = 
        let iterations = 
            let at = fetchAttribute xml "iterations"
            if at.IsNone then
                failwith "iteration attribute not found"
            else
                let s, v = Int32.TryParse (at.Value.ToString())
                if s then v else failwith "couldn't convert iterations into an int"
        let fates = 
            xml.Elements () 
            |> Seq.map                     
                (fun e -> 
                    let name = e.Attribute (xname "type")
                    let name = 
                        if name = null then
                            failwith "fate is missing the type attribute"
                        else
                            name.Value
                    match name with
                    | "survivor" -> 
                        let fateSurvivor = xmlFateSurvivor.fromXML e palettes
                        fate.Survivor fateSurvivor
                    | "escapee" ->
                        let fateEscapee = xmlFateEscapee.fromXML e palettes
                        fate.Escapee fateEscapee
                    | "fixpoint" ->
                        let fateFixpoint = xmlFateFixpoint.fromXML e palettes
                        fate.Fixpoint fateFixpoint
                    | "critical value" ->
                        let fateCriticalValue = xmlFateCriticalValue.fromXML e palettes it iterations
                        fate.CriticalValue fateCriticalValue
                    | "orbit check" ->
                        let fateOrbitCheck = xmlFateOrbitCheck.fromXML e palettes it
                        fate.OrbitCheck fateOrbitCheck
                    | _ -> 
                        failwith ("this fate not yet implemented" + name))
            |> Seq.toList
        {   xml = xml; 
            iterationMax = iterations; 
            fates = fates; }
    //to do:  don't pass z, iterations; instead pass iterator
    member x.checkFates (it : iterator) (z : complex) (iterations : int) (iterationMax : int) =
        let rec iter lst =
            match lst with
            | h::t -> 
                match h with 
                | fate.Escapee b ->
                    let escapee = b.escapee
                    let color = escapee.checkFate z iterations iterationMax
                    if color.IsSome then
                        color
                    else
                        iter t
                | fate.Fixpoint b ->
                    let target = b.fixpoint
                    let color = target.checkFate z iterations iterationMax
                    if color.IsSome then
                        color
                    else 
                        iter t
                | fate.CriticalValue b ->
                    let target = b.criticalValue
                    let color = target.checkFate z iterations iterationMax
                    if color.IsSome then
                        color
                    else 
                        iter t
                | fate.OrbitCheck o ->
                    let target = o.orbitChecker
                    let color = target.checkFate it iterationMax
                    if color.IsSome then
                        color
                    else
                        iter t
                | fate.Survivor b -> 
                    let survivor = b.survivor                   
                    let color = survivor.checkFate iterations iterationMax
                    if color.IsSome then
                        color
                    else 
                        iter t
            | [] -> None
        iter x.fates

//to do:  reference xmlInfo and xmlEquation records
type complexParameters = {
    name : string; 
    xml : XElement;
    }

type complexSeed = 
    | Mandelbrot
    | Julia

type bitmap = {
    width : int;
    height : int;
    rawImage : byte[]; } with 
    static member create w h =
        let pf = PixelFormats.Bgr32
        let rawStride = (w * pf.BitsPerPixel + 7) / 8
        let (max : int) = rawStride * h
        let (rawImage : byte[]) = Array.zeroCreate max    
        {   width = w;
            height = h;
            rawImage = rawImage;  }
    member t.setPixel (clr : Color) x y =
        let loc = 4 * (y * t.width + x)
        t.rawImage.[loc] <- clr.B
        t.rawImage.[loc + 1] <- clr.G
        t.rawImage.[loc + 2] <- clr.R
        ()
    //to do:  save
    member t.image () = 
        let pf = PixelFormats.Bgr32
        let w = t.width
        let h = t.height
        let rawStride = (w * pf.BitsPerPixel + 7) / 8
        let (max : int) = rawStride * h
        let bitmap = BitmapSource.Create (w, h, 96.0, 96.0, pf, null, t.rawImage, rawStride)
        //let bitmap = BitmapSource.Create (w, h, 300.0, 300.0, pf, null, t.rawImage, rawStride)
        let img = new Image()
        img.Width <- float t.width
        img.Height <- float t.height
        img.Source <- bitmap
        img

//to do:  bitmap record
//bresenham algorithm

//to do:  need info record
type plotObject = {
    xml : XElement;
    coords : plotCoordinates;
    palettes : plotPalettes;
    fates : plotFates;
    iterator : xmlIterator;
    bitmap : bitmap; } with 
    static member create (xml : XElement) =         
        let coords = xml.Element(xname "coordinates")
        let recCoordinates = plotCoordinates.create coords
        let palettes = xml.Element (xname "palettes")
        let plotPalettes = plotPalettes.create palettes
        let fates = xml.Element (xname "fates")
        let width = recCoordinates.coords.width
        let height = recCoordinates.coords.height
        let bitmap = bitmap.create width height
        let iterator = xmlIterator.create xml
        let plotFates = plotFates.create fates plotPalettes iterator.iterator
        {   xml = xml;
            coords = recCoordinates; 
            palettes = plotPalettes; 
            fates = plotFates; 
            iterator = iterator;
            bitmap = bitmap; }
    member x.iterate z = 
        let it = x.iterator.iterator
        match it with 
        | iterator.Mandelbrot i -> 
            i.init z         
        | iterator.Phoenix p -> 
            p.init z
        | iterator.NewtonBOF n ->
            n.init z
        | iterator.Newton n ->
            n.init z
        let iterationMax = x.fates.iterationMax
        let rec iter () =
            let iterations = 
                match it with 
                | iterator.Mandelbrot m -> m.iterations
                | iterator.Phoenix p -> p.iterations
                | iterator.NewtonBOF n -> n.iterations
                | iterator.Newton n -> n.iterations
            let iterations =
                match iterations with
                | Some i -> i
                | None -> failwith "bad value for iterations"
            let z = 
                match it with 
                | iterator.Mandelbrot m -> 
                    match m.z with 
                    | Some z -> z
                    | None -> failwith "bad value of z"
                | iterator.Phoenix p ->
                    match p.z with 
                    | Some z -> z
                    | None -> failwith "bad value of z"
                | iterator.NewtonBOF n ->
                    match n.z with
                    | Some z -> z
                    | None -> failwith "bad value of z"
                | iterator.Newton n ->
                    match n.z with
                    | Some z -> z
                    | None -> failwith "bad value of z"         
            let color = x.fates.checkFates it z iterations iterationMax
            match color with 
            | Some c -> 
                c
            | None -> 
                match it with
                | iterator.Mandelbrot m -> m.next ()
                | iterator.Phoenix p -> p.next ()
                | iterator.NewtonBOF n -> n.next ()
                | iterator.Newton n -> n.next ()
                iter ()
        iter ()
    member x.plot () =        
        let pixelsY = x.coords.coords.pixelDistanceY
        let pixelsX = x.coords.coords.pixelDistanceX
        let yMax = x.coords.coords.maxY
        let xMin = x.coords.coords.minX
        let upp = x.coords.coords.upp
        let mutable yCoordinate = 0.0 * 1.0<pixels>
        let switchXY = 
            match x.coords.swapXY with
            | Some b -> b
            | None -> false
        while yCoordinate <= pixelsY do
            let yUnit = yMax - (yCoordinate * upp)
            let mutable xCoordinate = 0.0 * 1.0<pixels>
            while xCoordinate <= pixelsX do
                let xUnit = xMin + (xCoordinate * upp)
                let z = 
                    if switchXY then
                        complex (float yUnit) (float xUnit)
                    else
                        complex (float xUnit) (float yUnit)
                let color = x.iterate z
                let xInt = int (float xCoordinate)
                let yInt = int (float yCoordinate)
                x.bitmap.setPixel color xInt yInt
                xCoordinate <- xCoordinate + 1.0<pixels>
            yCoordinate <- yCoordinate + 1.0<pixels>
        x.bitmap

let populateCbx (cbx : ComboBox) (xmlFolder : string) =
    cbx.DisplayMemberPath <- "name"
    let xml = XDocument.Load(xmlFolder + @"\complex.xml")
    let xname n = XName.op_Implicit(n)
    let plots = xml.Element(xname "plots")
    let xelem s el = new XElement(xname s, box el)
    let xatt a b = new XAttribute(xname a, b) |> box
    let xstr s = box s     
    //walk through xml elements and create records        
    for elem in plots.Elements() do
        //to do:  use xmlInfo and xmlEquation records
        let info = elem.Element (xname "info")
        let it = elem.Element (xname "iterator")
        let eqName, setName, fName =
            let functionNode = fetchElement it "function"
            let eqName = functionNode.Attribute (xname "name")
            let eqName = eqName.Value
            let fName = fetchElement info "name"
            let fName = fName.Value
            let setNode = fetchElement it "set"
            let setName = setNode.Attribute (xname "type")
            let setName = setName.Value
            eqName, setName, fName
        let (param : complexParameters) = { 
            name = "[" + eqName + "/" + setName + "] " + fName; 
            xml = elem;
            }
        cbx.Items.Add(param) |> ignore