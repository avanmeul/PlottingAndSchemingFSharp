module PlottingAndScheming.xml

open System
open System.Xml.Linq

let xname n = XName.op_Implicit (n)
let fetchElement (el : XElement) str = el.Element (xname str)
let xatt a b = new XAttribute(xname a, b) |> box

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

let fetchDouble : XElement -> string -> Double option = fetch Double.TryParse
//let fetchBoolean = fetch Boolean.TryParse
let fetchInt : XElement -> string -> Int32 option = fetch Int32.TryParse
let fetchAttribute (el : XElement) str = 
    let at = el.Attribute (xname str)
    if at = null then 
        None
    else
        Some at.Value