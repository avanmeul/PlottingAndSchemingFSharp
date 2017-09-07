module PlottingAndScheming.Scheme

(* Copyright (c) 2011, 2010, 2009, 2008, 2007, 2006 by André van Meulebrouck.  All rights reserved worldwide. *)

//master to do priority: 
(*

parse error:

'42.5-degrees ; this is due to bad regular expression for decimal numbers

This works:

(let* (
    (len-div-6 (lambda (len) (/ len 6)))
    (foo 
        (lambda (x) 
            (let ((z (len-div-6 x)))
                z))
    ))
    (foo 12))

This doesn't work:

(let* (
    (len-div-6 (lambda (len) (/ len 6)))
    (foo 
        (lambda (x) 
            (let ((x (len-div-6 x)))
                x))
    ))
    (foo 12))

This doesn't work:

(let (
    (len-div-6 (lambda (len) (/ len 6)))
    )
    (let (
        (foo 
            (lambda (x) 
                (let ((x (len-div-6 x)))
                    x)))
        )
        (foo 12)))

Next step:  convert to lambdas only.

Pattern:

(let ((x 3)) x)
((lambda (x) x) 3)

(let (
    (len-div-6 (lambda (len) (/ len 6)))
    )
    (let (
        (foo 
            (lambda (x) 
              ((lambda (x) x) (len-div-6 x))))
        )
        (foo 12)))

(let (
    (len-div-6 (lambda (len) (/ len 6)))
    )
    ((lambda (foo) (foo 12)) 
        (lambda (x) 
            ((lambda (x) x) (len-div-6 x)))))

; below shows conclusively that lambda is not implemented correctly

(   ;function
    (lambda (len-div-6)
        (   ;function
            (lambda (foo) 
                (foo 12))
            ;argument
            (lambda (x) 
                ((lambda (x) x) (len-div-6 x)))))
    ;
    (lambda (len) 
        (/ len 6))) ; => 2

; this works (just by alpha conversion on inner lambda)

(   ;function
    (lambda (len-div-6)
        (   ;function
            (lambda (foo) 
                (foo 12))
            ;argument
            (lambda (x) 
                ((lambda (z) z) (len-div-6 x)))))
    ;
    (lambda (len) 
        (/ len 6))) ; => 2

; this works:

((((lambda (x) (lambda (y) (lambda (z) ((x y) z)))) (lambda (x) (lambda (y) y))) 't) 'f)

((lambda (x) ((lambda (x) x) (/ 12 x))) 2)

; this fails:

(   ;function
    (lambda (y)
        (   ;function
            (lambda (x)     
                (   ;function
                    (lambda (x) x) 
                    ;arg
                    (y x))) 
            ;arg
            12)) 
    ;arg
    (lambda (len) (/ len 6)))
    
; this works:

(   ;function
    (lambda (y)
        (   ;function
            (lambda (x)     
                (   ;function
                    (lambda (z) z) 
                    ;arg
                    (y x))) 
            ;arg
            12)) 
    ;arg
    (lambda (len) (/ len 6))) ; => 2

; this works

(   ;function
    (lambda (y)
        (   ;function
            (lambda (x)                    
             (y x))
            ;arg
            12)) 
    ;arg
    (lambda (len) (/ len 6)))

; this works

(((lambda (x)
	(lambda (x)
		x)) 1) 2) ; => 2

; this works

(((lambda (x)
	(lambda (y)
		x)) 1) 2) ; => 1

; new test

(
    (lambda (x) 
        (
            (lambda (y) (y 3))
            (lambda (y) x)))
    4)
*)

//new vs. old

open PlottingAndScheming.xml
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

(*
Steps in building a LISP interpreter:

1) tokenizer
2) heap builder
3) heap printer
4) evaluator
*)

type scmToken =
    | LeftParen
    | RightParen
    | String of string
    | SingleQuote
    | Comment of string
    // | BlockComment of string
    | Dot
    | Sharp of string
    | Atom of string
    | Whitespace of string

type scmTokens = {
    mutable lastToken : scmToken option;
    mutable tokens : scmToken list; 
    } with
    static member create tokens = { lastToken = None; tokens = tokens }
    member x.get =
        match x.tokens with
        | [] -> 
            failwith "no more tokens"
        | h :: t ->
            x.lastToken <- Some h
            x.tokens <- t
            h
    member x.unget = 
        match x.lastToken with 
        | Some t -> 
            x.tokens <- (t :: x.tokens)
        | None ->
            failwith "can't unget, no tokens have been consumed"
    member x.peek =
        match x.tokens with
        | [] -> failwith "no more tokens"
        | h :: t -> h
    member x.tokensExist =
        x.tokens.Length > 0

type scmAtom = 
    | String of string
    | Symbol of string
    | Primitive of (scmCons -> scmBlock option -> scmObject)
    | Complex of complex
    | Float of float
    | Sharp of string
    | Int of int
and scmBlockType = 
    | Lambda
    | Let
and scmBlock = {
    block : scmCons;
    blockType : scmBlockType;
    mutable parent : scmBlock option; //to do:  discriminated union block + env
    mutable env : scmEnv option; } with
    static member create block blockType = {
        block = block;
        blockType = blockType;
        parent = None;
        env = None; }
    member x.clone () = {
        block = x.block;
        blockType = x.blockType;
        parent = x.parent;
        env = None; }
and scmThunk = {
    mutable parent : scmBlock option;
    mutable evaled : bool;
    mutable value : scmObject; } with
    static member create parent value = { 
        parent = parent;
        evaled = false;
        value = value; }
and scmObject = 
    | Atom of scmAtom
    | Block of scmBlock
    | Cons of scmCons
    | Thunk of scmThunk
and scmCons = {
    mutable car : scmObject option;
    mutable cdr : scmObject option; } with
    static member create () = 
        { car = None; cdr = None; }
and binding = {
    symbol : scmAtom;
    mutable evaled : bool;
    mutable value : scmObject; } with
    member x.toXml () = //to do
        let symbol = new XElement ((xname "symbol"), x.symbol)
        let value = new XElement ((xname "value"), x.value) //to do:  call print heap on this
        let binding = new XElement ((xname "binding"), symbol)
        let str = binding.ToString ()
        MessageBox.Show (str) |> ignore
        (*
        <binding>
            <symbol></symbol>
            <evaled></evaled>
            <value></value>
        </binding>        
        *)
        ()
and bindingNew = {
    symbol : scmAtom;
    mutable thunk : scmThunk; }
and scmEnv = 
    { mutable bindings : bindingNew list } with
    static member create () : scmEnv = { bindings = [] }
    member x.print () =
        x.bindings.Length
    member x.toXml () = //to do
        let foo = new XElement ((xname "foo"), "fido")
        x.bindings.Length 
    member x.add symbol value evaled (stack : scmStack) = 
        let parent = stack.head
        let thunk = scmThunk.create parent value
        let bindingNew = { symbol = symbol; thunk = thunk }
        x.bindings <- bindingNew :: x.bindings
    member x.tryFind sym =
        List.tryFind
            (fun b ->
                match b with
                | {symbol = s; thunk = v} ->
                    match s with
                    | scmAtom.Symbol s ->
                        s = sym
                    | _ -> failwith "bad symbol in binding")
            x.bindings       
and symTable = { mutable symbols : scmAtom list } with
    static member create = { symbols = [] }
    member x.getSymbol name =
        let res =
            List.tryFind 
                (fun n -> 
                    match n with
                    | scmAtom.Symbol s -> s = name
                    | _ -> false)
                x.symbols
        match res with
        | None -> 
            let sym = scmAtom.Symbol name
            x.symbols <- sym :: x.symbols
            sym
        | Some s -> 
            s
    member x.tryFind name = 
        List.tryFind 
            (fun n -> 
                match n with
                | scmAtom.Symbol s -> s = name
                | _ -> false)
            x.symbols
and scmStack = {
    mutable frames : scmBlock list; } with 
    static member create () = {
        frames = []; }
    member x.head =
        match x.frames with
        | [] -> None
        | h :: t -> Some h
    member x.add frame =
        x.frames <- frame :: x.frames
    member x.drop =
        x.frames <- (List.tail x.frames)
    member x.pop n =
        for i = 1 to n do
            x.frames <- (List.tail x.frames)
    
type globalEnv = {
    top : scmEnv; } with 
    static member create env = {
        top = env; }
    (*
    environments are pointed to by the stack but are threaded in a heap-like fashion
    hence, we can walk through all parents just by knowing an environment (without regard to the stack)
    *)
    member x.tryFind sym (env : scmBlock option) =
        //contract:  look through current environment
        //on failure, check parents
        //on failure, check global environment
        //to do:  create a lookup that goes by frames to skip and slot to check
        //create a binding record that also always memoizing the frame/slot location to expedite 
        //subsequent look ups
        let rec iter (parent : scmBlock option) = 
            match parent with
            | None ->
                x.top.tryFind sym
            | Some l ->
                let localEnv = l.env 
                if localEnv.IsSome then
                    let localEnv = localEnv.Value
                    let localBind = localEnv.tryFind sym
                    if localBind.IsNone then
                        iter l.parent
                    else
                        localBind
                else 
                    iter l.parent
        iter env
    member x.print () = //to do:  first list how many elements, then display them and levels
        x.top.print ()
        
let stack = scmStack.create ()
            
let nil = scmCons.create ();

let (|Whitespace|_|) (x : char) =
    match x with
    | '\t' | ' ' | '\r' | '\n' -> Some ()
    | _ -> None

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

let (|ComplexNumber|_|) (s : string) =
    schemeParseComplex s

let schemeParseFloat f =
    let pat = "^([+|-]{0,1}[0-9]+[.]{1}[0-9]+)([e]{1}[+|-]{0,1}[0-9]+){0,1}"
    if Regex.IsMatch (f, pat) then
        let r = Regex pat
        let s = r.Match f
        let n = s.Groups
        //to do:  don't repeat same logic in match statements
        match n.Count with
        | 1 ->
            let f = n.[1]
            let s, v = Double.TryParse (f.Value)
            if s then Some v else None
        | 2 ->
            let f = n.[1]
            let e = n.[2]
            let s, v = Double.TryParse (f.Value + e.Value)
            if s then Some v else None
        | 3 ->
            let f = n.[1]
            let e = n.[2]
            let s, v = Double.TryParse (f.Value + e.Value)
            if s then Some v else None
        | _ -> 
            failwith "bad floating point number"
    else
        None

let (|FloatNumber|_|) (s : string) =
    schemeParseFloat s

let schemeParseInt i =
    let pat = "[+|-]{0,1}[0-9]+"    
    if Regex.IsMatch (i, pat) then
        let r = Regex pat
        let s = r.Match i
        let s, v = Int32.TryParse i
        if s then Some v else None
    else
        None

let (|IntegerNumber|_|) (s : string) =
    schemeParseInt s

type parse = {
    mutable string : int option; 
    mutable atom : int option;    
    mutable dot : int option; 
    mutable sharp : int option;
    mutable comment : int option; //to do
    mutable blockComments : int * int option; //to do
    mutable parens : int;} with
    static member create = {
        string = None;
        atom = None;        
        dot = None; 
        sharp = None;
        comment = None;
        blockComments = -1, None;
        parens = -1; }  
    member x.incParenCount =
        x.parens <- x.parens + 1
    member x.decParenCount =
        if x.parens = -1 then
            failwith "too many closing parens"
        else
            x.parens <- x.parens - 1
    member x.incBlockComments =
        failwith "not implemented yet" //to do
    member x.decBlockComments =
        failwith "not implemented yet" //to do

(*
test case from EoPL 1.3.1
((lambda (x)
  (list x (list (quote quote) x)))
 (quote (lambda (x) 
	(list x (list (quote quote) x)))))
*)

//below:  test cases
(*
'(a b . (c)) ;dr scheme:  
'(a b .(c))
(cdr '(.....a ... . ....c))
'(.a . 'b)
'(a. . 'b)
'(a . 'b
(car' (a b c))
'(a .'b)
'(a. .'b)
'(.a'b)
*)
let tokenize (str : string) =
    let max = str.Length - 1
    let inProgress = parse.create
    let substring (str : string) start finish =
        let len = finish - start + 1          
        str.Substring (start, len)
    let rec iter i acc =
        let processDot () = //to do:  add sharp to this
            match 
                inProgress.atom, 
                inProgress.dot, 
                inProgress.string,
                inProgress.comment with
            | _, _, _, Some c -> //comments swallow everything
                iter (i + 1) acc
            | _, _, Some s, None -> //strings swallow everything but comments
                iter (i + 1) acc
            | Some a, _, None, None ->
                iter (i + 1) acc
            | None, Some d, None, None ->
                //we've hit more than one dot in a row
                inProgress.dot <- None
                inProgress.atom <- Some d             
                iter (i + 1) acc
            | None, None, None, None ->
                inProgress.dot <- Some i
                iter (i + 1) acc
        let processSingleQuote () = //to do:  add sharp processing
            match 
                inProgress.atom, 
                inProgress.dot, 
                inProgress.string,
                inProgress.comment with
            | _, _, _, Some c -> //comments swallow everything
                iter (i + 1) acc
            | Some a, None, None, None -> //strings swallow everything but comments
                //(car' (a b c))
                let s = substring str a (i - 1)
                let acc = scmToken.SingleQuote :: (scmToken.Atom s) :: acc
                inProgress.atom <- None
                iter (i + 1) acc
            | None, Some d, None, None ->
                //'(a .'b)
                //'(a. .'b)              
                let s = substring str d (i - 1)
                inProgress.dot <- None
                let acc = 
                    scmToken.SingleQuote ::                             
                    scmToken.Dot :: 
                    acc
                iter (i + 1) acc
            | Some a, Some d, None, None -> //dot + atom + single quote
                //'(.a'b)
                inProgress.atom <- None
                inProgress.dot <- None
                let s = substring str a (i - 1)                        
                let acc = 
                    scmToken.SingleQuote ::                             
                    scmToken.Dot :: 
                    (scmToken.Atom s) :: 
                    acc
                iter (i + 1) acc
            | None, _, None, None -> 
                //'(a . 'b)                             
                iter (i + 1) (scmToken.SingleQuote :: acc)
            | _, _, Some s, None ->
                iter (i + 1) acc
        if i > max then
            if inProgress.parens <> -1 then
                failwith "unbalanced parens"
            else //finish any unfinished business
                match 
                    inProgress.atom, 
                    inProgress.dot, 
                    inProgress.string,
                    inProgress.comment,
                    inProgress.sharp with
                | _, _, Some s, None, None ->
                    failwith "unterminated string"
                | Some a, Some d, None, None, None ->
                    let s = substring str a (i - 1)
                    (scmToken.Atom s) ::
                    scmToken.Dot ::
                    acc
                | Some a, None, None, None, None -> //atom:  might even end in a dot (or dots)
                    let s = substring str a (i - 1)
                    ((scmToken.Atom s) :: acc)
                | None, Some d, None, None, None ->
                    failwith "dot in progress was never completed"
                | None, None, None, None, None ->
                    acc
                | _, _, _, None, Some s ->
                    let s = substring str s (i - 1)
                    (scmToken.Sharp s) :: acc
                | _, _, _, Some c, _ ->
                    let s = substring str c (i - 1)
                    (scmToken.Comment s) :: acc
                |> List.rev               
        else      
            match str.[i] with
            | '"' -> //strings can swallow everything except block comments
                match inProgress.string with
                | None -> 
                    inProgress.string <- Some i
                    inProgress.dot <- None
                    inProgress.atom <- None
                    inProgress.sharp <- None
                    iter (i + 1) acc
                | Some s ->                    
                    let s = substring str s i
                    let acc = (scmToken.String s) :: acc
                    inProgress.string <- None
                    iter (i + 1) acc
            | Whitespace -> 
                //'(.a . 'b)
                //'(a. . 'b)
                //'(a . 'b)
                //to do:  should there be a whitespace token that gets turned on?
                match 
                    inProgress.atom, 
                    inProgress.dot, 
                    inProgress.string,
                    inProgress.comment,
                    inProgress.sharp with
                | Some a, Some d, None, None, None ->            
                    inProgress.atom <- None
                    inProgress.dot <- None
                    let diff = a - d
                    if diff > 1 then
                        //atom + dot
                        let s = substring str a (i - 1)                        
                        let acc = (scmToken.Atom s) :: scmToken.Dot :: acc
                        iter (i + 1) acc
                    else
                        //atom 
                        let s = substring str (min a d) (i - 1)
                        let acc = (scmToken.Atom s) :: acc
                        iter (i + 1) acc
                | Some a, None, None, None, None ->
                    //atom
                    let s = substring str a (i - 1)
                    inProgress.atom <- None
                    iter (i + 1) ((scmToken.Atom s) :: acc)   
                | None, Some d, None, None, None ->
                    iter (i + 1) acc
                | None, None, None, None, None ->
                    iter (i + 1) acc
                | _, _, Some s, None, None ->
                    iter (i + 1) acc
                | _, _, _, None, Some s ->
                    let s = substring str s (i - 1)
                    let acc = (scmToken.Sharp s) :: acc
                    inProgress.sharp <- None
                    iter (i + 1) acc
                | _, _, _, Some c, _ ->
                    if str.[i] = '\r' then
                        let s = substring str c (i - 1)
                        let acc = (scmToken.Comment s) :: acc
                        inProgress.comment <- None
                        iter (i + 1) acc
                    else 
                        iter (i + 1) acc
            | '(' ->                
                match 
                    inProgress.atom, 
                    inProgress.dot, 
                    inProgress.string, 
                    inProgress.comment,
                    inProgress.sharp with
                | _, _, _, Some c, _ -> //comments swallow everything
                    iter (i + 1) acc
                | _, _, Some s, None, None -> //strings swallow everything except comments
                    iter (i + 1) acc
                | Some a, Some d, None, None, None -> //atom with dots
                    inProgress.incParenCount
                    let least = min a d
                    let s = str.Substring(least, i - least)
                    let acc = (scmToken.LeftParen) :: (scmToken.Atom s) :: acc
                    inProgress.atom <- None
                    iter (i + 1) acc
                | Some a, None, None, None, None -> //atom
                    inProgress.incParenCount
                    let s = substring str a (i - 1)
                    let acc = 
                        scmToken.LeftParen :: 
                        (scmToken.Atom s) ::
                        acc
                    inProgress.atom <- None
                    iter (i + 1) acc
                | None, Some d, None, None, None ->
                    inProgress.incParenCount 
                    inProgress.dot <- None
                    let acc = 
                        scmToken.LeftParen ::
                        scmToken.Dot ::
                        acc
                    iter (i + 1) acc
                | None, None, None, None, None ->
                    inProgress.incParenCount
                    let acc = scmToken.LeftParen :: acc
                    iter (i + 1) acc
                | _, _, _, None, Some s -> //vector
                    let s = substring str s (i - 1)
                    let acc = scmToken.LeftParen :: scmToken.Sharp s :: acc
                    inProgress.incParenCount
                    inProgress.sharp <- None
                    iter (i + 1) acc
            | ')' ->
                match 
                    inProgress.atom, 
                    inProgress.dot, 
                    inProgress.string, 
                    inProgress.comment,
                    inProgress.sharp with
                | _, _, _, Some c, _ -> //comments swallow everything
                    iter (i + 1) acc
                | _, _, Some s, None, None -> //strings swallow everything but comments
                    iter (i + 1) acc
                | None, Some d, None, None, None ->
                    if i - d = 1 then failwith "bad cdr field in dotted pair"
                    let s = substring str d (i - 1)
                    let acc = scmToken.RightParen :: (scmToken.Atom s) :: acc
                    inProgress.decParenCount
                    inProgress.dot <- None
                    iter (i + 1) ((scmToken.Atom s) :: acc)
                | Some a, Some d, None, None, None ->
                    //atom dot mixture
                    inProgress.atom <- None
                    inProgress.dot <- None
                    inProgress.decParenCount
                    let diff = a - d
                    if diff > 1 then
                        //atom + dot
                        let s = substring str a (i - 1)                        
                        let acc = 
                            scmToken.RightParen :: 
                            (scmToken.Atom s) :: 
                            scmToken.Dot :: 
                            acc
                        iter (i + 1) acc
                    else
                        //atom 
                        let s = substring str (min a d) (i - 1)
                        let acc = 
                            scmToken.RightParen :: 
                            (scmToken.Atom s) :: 
                            acc
                        iter (i + 1) acc                    
                | Some a, _, None, None, None ->
                    inProgress.decParenCount
                    inProgress.atom <- None
                    let s = substring str a (i - 1)
                    let acc = scmToken.RightParen :: (scmToken.Atom s) :: acc
                    iter (i + 1) acc
                | _, _, _, None, Some s ->
                    let s = substring str s (i - 1)
                    let acc = scmToken.RightParen :: (scmToken.Sharp s) :: acc
                    inProgress.decParenCount
                    inProgress.sharp <- None
                    iter (i + 1) acc
                | None, None, None, None, None ->
                    inProgress.decParenCount
                    let acc = scmToken.RightParen :: acc
                    iter (i + 1) acc
            | '.' -> processDot ()           
            | '\'' -> processSingleQuote ()
            | '#' -> //sharp sign
                //to do:  add comment
                match 
                    inProgress.atom, 
                    inProgress.dot, 
                    inProgress.string, 
                    inProgress.sharp with
                | _, _, Some s, _ -> //strings swallow
                    iter (i + 1) acc
                | Some a, _, _, _ -> //atoms swallow
                    iter (i + 1) acc 
                | None, _, None, Some s ->
                    failwith "bad sharp sign expression"
                | None, _, None, None ->
                    inProgress.sharp <- Some (i + 1)
                    iter (i + 1) acc
            | ';' -> //comment
                match 
                    inProgress.string,
                    inProgress.comment with
                | Some s, _ -> //strings swallow
                    iter (i + 1) acc
                | None, None ->
                    let next = i + 1
                    inProgress.comment <- Some next
                    iter next acc
                | None, Some c ->
                    iter (i + 1) acc                       
            | _ -> //atom
                match 
                    inProgress.atom, 
                    inProgress.dot, 
                    inProgress.string,
                    inProgress.comment,
                    inProgress.sharp with
                | _, _, Some s, None, _ ->
                    iter (i + 1) acc
                | None, Some d, None, None, None ->
                    //atom with dot(s) in it
                    inProgress.atom <- Some i
                    if i - d = 1 then //this is an atom that starts with a dot
                        inProgress.dot <- None
                        inProgress.atom <- Some d
                    iter (i + 1) acc
                | Some a, Some d, _, None, None ->
                    iter (i + 1) acc
                | Some a, None, None, None, None ->
                    iter (i + 1) acc
                | None, None, None, None, None ->
                    inProgress.atom <- Some i
                    iter (i + 1) acc
                | _, _, None, None, Some s ->
                    iter (i + 1) acc
                | _, _, _, Some c, _ ->
                    iter (i + 1) acc
    iter 0 []    

let symbolTable = symTable.create

let topLevel = globalEnv.create (scmEnv.create ())

//to do:  strip comments from token stream before building the heap
let buildHeap (tokens : scmTokens) =
    let rec recur () = 
        let rec getToken () =
            let tok = tokens.get
            match tok with 
            | scmToken.Comment s ->
                getToken ()
            | _ -> tok
        let rec peekToken () =
            let tok = tokens.peek
            match tok with 
            | scmToken.Comment s ->
                tokens.get |> ignore
                peekToken ()
            | _ -> tok
        //let tok = tokens.get
        let tok = getToken ()
        match tok with
        | scmToken.String s ->
            scmObject.Atom (scmAtom.String s)
//        | scmToken.Comment c ->            
//            recur ()
        | scmToken.Atom a ->
            //see what kind of atom it is
            let a : scmAtom =
                match a with
                | ComplexNumber c -> scmAtom.Complex c
                | FloatNumber f -> scmAtom.Float f
                | IntegerNumber i -> scmAtom.Int i
                | _ -> scmAtom.Symbol a
            scmObject.Atom a
        | scmToken.Sharp s ->
            scmObject.Atom (scmAtom.Sharp s)
        | scmToken.SingleQuote ->
            let cell = scmCons.create ()
            let sym = symbolTable.getSymbol "quote"
            cell.car <- Some (scmObject.Atom sym)
            let next = scmCons.create ()
            cell.cdr <- Some (scmObject.Cons next)
            let node = recur ()
            next.car <- Some node
            scmObject.Cons cell
        | scmToken.LeftParen ->
            let tok = tokens.peek
            if tok = scmToken.RightParen then
                getToken () |> ignore
                scmObject.Cons nil
            else
                let mutable cell = scmCons.create ()
                let firstCell = cell
                //to do:  use recursion and get rid of mutation
                let mutable keepGoing = true
                while tokens.tokensExist && keepGoing do
                    cell.car <- Some (recur ())
                    let tok = peekToken ()                    
                    match tok with 
                    | scmToken.RightParen ->
                        getToken () |> ignore
                        keepGoing <- false
                    | scmToken.Dot ->
                        getToken () |> ignore
                        cell.cdr <- Some (recur ())
                        let tok = getToken ()
                        if tok <> scmToken.RightParen then
                            failwith "dot expression not properly ended"
                        keepGoing <- false
                    | _ ->
                        let newCell = scmCons.create ()
                        cell.cdr <- Some (scmObject.Cons newCell)
                        cell <- newCell
                scmObject.Cons firstCell
        | _ -> 
            failwith "not implemented yet"
    recur ()

let rec printHeap heap =
    match heap with
    | scmObject.Atom a ->
        match a with 
        | scmAtom.Symbol s ->
            s
        | scmAtom.Float f ->
            f.ToString ()
        | scmAtom.Int i ->
            i.ToString ()
        | scmAtom.Complex c ->
            c.ToString ()
        | scmAtom.String s ->
            s
        | scmAtom.Sharp s ->
            "#" + s
        | scmAtom.Primitive p ->           
            "primitive function:  " + p.ToString ()
    | scmObject.Thunk t ->
        let t = t.value
        printHeap t
    | scmObject.Block b ->
        match b.blockType with 
        | scmBlockType.Lambda ->
            "lambda"
        | scmBlockType.Let -> 
            "let"
    | scmObject.Cons c ->
        //walk across a list
        let mutable s = "("
        let mutable cell = c 
        let mutable keepGoing = true
        let mutable firstCell = true
        cell <- c
        while keepGoing do
            match cell.car, cell.cdr with
            | None, None ->
                if firstCell then
                    s <- s + ")"
                else
                    failwith "bad cons"
                keepGoing <- false
            | Some h, Some t ->
                match t with
                | scmObject.Atom _ ->                            
                    s <- s + (printHeap h) + " . " + (printHeap t) + ")"
                    keepGoing <- false
                | scmObject.Block b ->
                    failwith "not implemented yet"
                | scmObject.Cons c -> 
                    s <- s + (printHeap h) + " " 
                    cell <- c
                | scmObject.Thunk t ->
                    let t = t.value
                    s <- s + (printHeap t)
            | Some h, None ->
                s <- s + (printHeap h) + ")"
                keepGoing <- false
            | None, _ ->
                failwith "this should never happen:  bad list"
            firstCell <- false
        s

let scmTrue = scmObject.Atom (scmAtom.Sharp "t")
let scmFalse = scmObject.Atom (scmAtom.Sharp "f")

//below:  test cases
(*
((car (cons cdr car)) '(a b c)) ; => '(b c)
((car '(cdr car)) '(a b c)) ; => error
((lambda () 3))
(((lambda (x) (lambda (y) x)) #t) #f)
((((lambda (x) (lambda (y) (lambda (z) z))) 'x) 'y) 'z) ;=> sometimes fails with bad lookup on z
((lambda (x) x 2) 3) ;return last evaluated object
((lambda (x y ) (cons x y)) 'a 'b)
(((lambda (x) (x x)) (lambda (x) x)) 3) ;=> 3 in applicative order, stack overflow in normal order
(let ((x 3) (y 4)) (cons x y))
(let ((x 3) (y 4)) (+ x y))
(let ((x 3) (y (cons x x))) y) ;=> to do:  save env with bindings
(let ((x 3)) 
    (let* ((y x) (z y)) z))
(let* ((x 3) (y x)) y)
(let* ((x 3) (y (+ x 1))) y)
(let* ((x (+ y 2)) (y 3)) x)
(letrec ((y (+ x 1)) (x 3)) y)
(letrec 
    ((fact 
        (lambda (n) 
            (if (zero? n) 
                1 
                (* n (fact (- n 1)))))))
    (fact 5))
*)*)

let rec eval 
    (heap : scmObject) 
    (block : scmBlock option) 
    =
    match heap with
    | scmObject.Atom a -> 
        //atoms are self-evaluating, except for symbols (which get looked up)
        match a with
        | scmAtom.Symbol s ->
                let res = topLevel.tryFind s block
                match res with 
                | None -> 
                    failwith ("lookup failure on symbol " + s)
                | Some {symbol = s; thunk = t} ->
                    if t.evaled then
                        t.value
                    else 
                        let parent = t.parent
                        let v = eval t.value parent                     
                        res.Value.thunk.value <- v
                        res.Value.thunk.evaled <- true
                        v
        | scmAtom.Sharp s ->
            match s with 
            | "t" -> scmTrue
            | "f" -> scmFalse
            | _ -> failwith "bad sharp element"
        | _ -> 
            scmObject.Atom a
    | scmObject.Thunk t ->
        if t.evaled then
            t.value
        else 
            let parent = t.parent
            let v = eval t.value parent                   
            t.value <- v
            t.evaled <- true
            v
    | scmObject.Block b ->
        scmObject.Block b
    | scmObject.Cons c -> //function call
        match c.car, c.cdr with
        | None, None ->
            scmObject.Cons nil
        | Some func, Some args ->
            let evaledFunc = eval func block
            match evaledFunc, args with            
            | scmObject.Atom a, scmObject.Cons c ->
                match a with 
                | scmAtom.Primitive p ->
                    p c block
                | scmAtom.Symbol s ->
                    failwith ("bad function :  " + s)      
                | _ -> 
                    failwith "bad function"                         
            | scmObject.Block f, scmObject.Cons a -> //closure or block
                match f.blockType with 
                | scmBlockType.Lambda ->
                    let lambda = f.block
                    match lambda.car with 
                    | Some (scmObject.Cons c) ->
                        let env = scmEnv.create ()
                        //populate the environment
                        let rec popEnv paramList argList =
                            match 
                                (paramList : scmObject option), 
                                (argList : scmObject option) with
                                | None, None -> 
                                    ()
                                |   Some (scmObject.Cons { car = Some (scmObject.Atom (scmAtom.Symbol hp)); cdr = tp }), 
                                    Some (scmObject.Cons { car = Some ha; cdr = ta }) -> 
                                    //I caught this trying to bind parameter len to symbol x instead of a thunk
                                    //let t = scmThunk.create (Some f) ha
                                    env.add 
                                        (symbolTable.getSymbol hp) 
                                        ha 
                                        //(scmObject.Thunk t) 
                                        false //evaled?
                                        stack //to do:  why pass this if not passing a frame?
                                    popEnv tp ta
                                | Some (scmObject.Cons { car = h; cdr = t }), None ->
                                    failwith "not enough arguments provided"
                                | None, Some (scmObject.Cons {car = h; cdr = t}) ->
                                    failwith "too many arguments provided"
                                | _, _ ->
                                    failwith "bad arguments to a function"
                        popEnv lambda.car (Some args)
                        let frame = f.clone ()
                        frame.env <- Some env
                        stack.add frame
                        let body = evalBody lambda.cdr frame
                        stack.drop 
                        body
                    | _ -> failwith "bad lambda parameter list"
                | scmBlockType.Let ->
                    failwith "let block type not implemented yet"
            | scmObject.Cons f, scmObject.Cons a ->             
                failwith "non-function in function position"
            | _, scmObject.Atom a ->
                failwith "bad argument to a function"
            | _, scmObject.Thunk t ->
                failwith "thunk not implemented yet 2"
            | scmObject.Thunk t, scmObject.Cons c ->
                failwith "thunk not implemented yet 3"
            | _, scmObject.Block b ->
                failwith "block as arg to block:  not implemented yet"
        | Some func, None -> //lambda with no arguments
            let evaledFunc = eval func block         
            match evaledFunc with
            | scmObject.Atom a ->
                match a with 
                | scmAtom.Primitive p ->
                    p nil None
                | scmAtom.Symbol s ->
                    failwith ("bad function :  " + s)      
                | _ -> 
                    failwith "bad function"     
            | scmObject.Block f ->
                match f.blockType with 
                | scmBlockType.Lambda ->
                    let lambda = f.block
                    match lambda.car with 
                    | Some (scmObject.Cons c) ->
                        let env = scmEnv.create ()
                        let rec popEnv paramList argList =
                            match 
                                (paramList : scmObject option), 
                                (argList : scmObject option) with
                                | None, None -> 
                                    ()
                                |   Some (scmObject.Cons { car = Some (scmObject.Atom (scmAtom.Symbol hp)); cdr = tp }), 
                                    Some (scmObject.Cons { car = Some ha; cdr = ta }) -> 
                                    env.add 
                                        (symbolTable.getSymbol hp) 
                                        ha
                                        false
                                        stack
                                    popEnv tp ta
                                | Some (scmObject.Cons { car = h; cdr = t }), None ->
                                    ()
                                | None, Some (scmObject.Cons {car = h; cdr = t}) ->
                                    failwith "too many arguments provided"
                                | _, _ ->
                                    failwith "bad arguments to a function"
                        popEnv lambda.car None
                        f.env <- Some env
                        let frame = f.clone ()
                        stack.add frame
                        let body = evalBody lambda.cdr f
                        stack.drop 
                        body
                    | _ -> failwith "bad lambda parameter list"
                | scmBlockType.Let ->
                    failwith "let block type not implemented yet"
            | scmObject.Cons f ->                        
                failwith "non-function in function position"
            | scmObject.Thunk t ->
                failwith "thunk not implemented yet 1"
        | _ -> failwith "not implemented yet"
and evalBody body block =
    let rec iter body lastVal =
        match body with 
        | None ->
            match lastVal with
            | None ->
                failwith "nothing found in block body"
            | Some v -> 
                v
        | Some (scmObject.Cons { car = Some h; cdr = t } )->
            //to do:  make sure block is really Some thing
            iter t (Some (eval h (Some block)))
        | _ -> 
            failwith "bad block body"
    iter body None

let quote (args : scmCons) (block : scmBlock option) = 
    match args.car with
    | Some c -> c
    | None -> failwith "bad argument to quote"

let car (args : scmCons) (block : scmBlock option) = 
    match args.car with
    | Some c -> 
        let e = eval c block
        match e with
        | scmObject.Cons { car = Some c; cdr = _ } ->
            c
        | _ -> failwith "bad argument to car"
    | None -> failwith "bad argument to car"

let cdr (args : scmCons) (block : scmBlock option) = 
    match args.car with
    | Some c -> 
        let e = eval c block
        match e with
        | scmObject.Cons { car = _; cdr = Some c } ->
            c
        | scmObject.Cons { car = _; cdr = None } -> 
            scmObject.Cons nil
        | _ -> failwith "bad argument to cdr"
    | None -> failwith "bad argument to cdr"

let cons (args : scmCons) (block : scmBlock option) = 
    match args.car, args.cdr with
    | Some h, Some (scmObject.Cons {car = Some t; cdr = None}) ->
        let e1, e2 = eval h block, eval t block
        let cell = scmCons.create ()
        cell.car <- Some e1
        cell.cdr <- Some e2
        scmObject.Cons cell
    | _, _ -> failwith "bad argument to cons"

let lambda (args : scmCons) (block : scmBlock option) = 
    //let parent = stack.head
    let parent = block
    scmObject.Block { 
        block = args; 
        blockType = scmBlockType.Lambda; 
        parent = parent; 
        env = None; }

let funCar (x : scmObject option) = 
    match x with 
    | Some (scmObject.Cons { car = Some h; cdr = _ }) -> 
        h
    | _ -> 
        failwith "bad argument to car"

let funCdr (x : scmObject option) = 
    match x with 
    | Some (scmObject.Cons { car = _; cdr = t }) -> 
        t
    | _ -> 
        failwith "bad argument to car"

let funCadr (x : scmObject option) = 
    x |> funCdr |> funCar

let funNull (x : scmObject option) = 
    match x with 
    | None -> true
    | _ -> false

let funSymbolName (x : scmObject option) = 
    match x with 
    | Some (scmObject.Atom (scmAtom.Symbol s)) -> Some s
    | _ -> None

let scmLet (args : scmCons) (block : scmBlock option) = 
    //find out what the head of the stack is
    let top = stack.head // this is the head of stack to use for thunks    
    let parent : scmBlock option = top
    let bindings, body = 
        match scmObject.Cons args with
        | (scmObject.Cons { car = Some h; cdr = Some t }) ->
            h, t
        | _ -> failwith "let:  malformed"    
    //create a new environment
    let env = scmEnv.create ()
    //walk across bindings and evaluate RHS for each
    let rec iter (bindings : scmObject option) =
        if funNull bindings then
            ()
        else 
            let el = Some (funCar bindings)
            let symbol = Some (funCar el)
            let evaledArg = (funCadr el)
            //let thunk = scmThunk.create top evaledArg
            let thunk = scmThunk.create block evaledArg
            match funSymbolName symbol with
            | None -> 
                failwith "bad symbol in let binding "
            | Some s -> 
                env.add
                    (symbolTable.getSymbol s) 
                    (scmObject.Thunk thunk)
                    false
                    stack
                iter (funCdr bindings)
    iter (Some bindings)
    //create block
    let blockBody = 
        match body with
        | scmObject.Cons c -> c
        | _ -> failwith "bad body in let"
    let block = {        
        block = blockBody;
        blockType = scmBlockType.Let; 
        parent = parent; 
        env = Some env; }
    //create stack frame
    //let frame = scmStackFrame.create block
    let frame = block
    //push stack frame
    stack.add frame
    //eval body    
    let body = evalBody (Some body) block
    //drop stack frame
    stack.drop
    //return the body
    body

let scmLetStar (args : scmCons) (block : scmBlock option) = 
    //find out what the head of the stack is
    let top = stack.head // this is the head of stack to use for thunks    
    let parent : scmBlock option = top
    let bindings, body = 
        match scmObject.Cons args with
        | (scmObject.Cons { car = Some h; cdr = Some t }) ->
            h, t
        | _ -> failwith "let:  malformed"
    let pushes = ref 0
    //walk across bindings and evaluate RHS for each
    let rec iter (bindings : scmObject option) parent =
        if funNull bindings then
            ()
        else 
            //create a new environment
            let env = scmEnv.create ()
            let el = Some (funCar bindings)
            let symbol = Some (funCar el)
            let evaledArg = (funCadr el)            
            //create block
            let blockBody = 
                match body with
                | scmObject.Cons c -> c
                | _ -> failwith "bad body in let"
            //let thunk = scmThunk.create parent evaledArg
            let block = {   
                block = blockBody;
                blockType = scmBlockType.Let; 
                parent = parent; 
                env = Some env; }
            let thunk = scmThunk.create parent evaledArg
            //create stack frame
            let frame = block
            //push stack frame
            stack.add frame
            pushes := pushes.Value + 1
            match funSymbolName symbol with
            | None -> 
                failwith "bad symbol in let binding"
            | Some s -> 
                env.add
                    (symbolTable.getSymbol s) 
                    (scmObject.Thunk thunk)
                    false
                    stack
                iter (funCdr bindings) (Some block)
    iter (Some bindings) parent
    //iter (Some bindings) stack.head
    //eval body
    let body = evalBody (Some body) stack.head.Value
    //drop stack frame
    stack.pop pushes.Value
    //return the body
    body

let scmList (args : scmCons) (block : scmBlock option) = 
    let rec iter (args : scmCons) (res : scmCons) =
        match args with
        | {car = Some h; cdr = t} ->
            //cons this on
            res.car <- Some (eval h block)
            match t with
            | None ->
                res.cdr <- None
                ()
            | Some (scmObject.Cons c) ->
                let newCons = scmCons.create ()
                res.cdr <- Some (scmObject.Cons newCons)
                iter c newCons
            | _ -> failwith "bad argument list given to list function"
        | _ -> failwith "bad argument to list"
    let res = (scmCons.create ())
    iter args res
    (scmObject.Cons res)

type IntFloat = 
    | Int of int
    | Float of float

let scmPlus (args : scmCons) (block : scmBlock option) = 
    let rec iter (args : scmCons) (res : IntFloat) =
        match args with
        | {car = Some h; cdr = t} ->
            let num = eval h block
            let res = 
                match res, num with
                | IntFloat.Float f1, scmObject.Atom (scmAtom.Float f2) ->
                    IntFloat.Float (f1 + f2)
                | IntFloat.Int i1, scmObject.Atom (scmAtom.Float f2) ->
                    IntFloat.Float ((float i1) + f2)
                | IntFloat.Int i1, scmObject.Atom (scmAtom.Int i2) ->
                    IntFloat.Int (i1 + i2)
                | IntFloat.Float f1, scmObject.Atom (scmAtom.Int i2) ->
                    IntFloat.Float (f1 + (float i2))
                //to do:  support complex numbers
                | _, _ ->
                    failwith "bad argument to + function"
            match t with
            | None ->
                res
            | Some (scmObject.Cons c) ->
                iter c res
            | _ -> failwith "bad argument list given to list function"
        | _ -> failwith "bad argument to list"
    let res = iter args (IntFloat.Int 0)
    match res with
    | IntFloat.Int i -> 
        scmObject.Atom (scmAtom.Int i)
    | IntFloat.Float f -> 
        scmObject.Atom (scmAtom.Float f)

let scmMinus (args : scmCons) (block : scmBlock option) = 
    let rec iter (args : scmCons) (res : IntFloat option) =
        match args with
        | {car = Some h; cdr = t} ->
            let num = eval h block
            let res = 
                match res, num with
                | None, scmObject.Atom (scmAtom.Float f2) ->
                    Some (IntFloat.Float f2)
                | None, scmObject.Atom (scmAtom.Int i2) ->
                    Some (IntFloat.Int i2)
                | Some (IntFloat.Float f1), scmObject.Atom (scmAtom.Float f2) ->
                    Some (IntFloat.Float (f1 - f2))
                | Some (IntFloat.Int i1), scmObject.Atom (scmAtom.Float f2) ->
                    Some (IntFloat.Float ((float i1) - f2))
                | Some (IntFloat.Int i1), scmObject.Atom (scmAtom.Int i2) ->
                    Some (IntFloat.Int (i1 - i2))
                | Some (IntFloat.Float f1), scmObject.Atom (scmAtom.Int i2) ->
                    Some (IntFloat.Float (f1 - (float i2)))
                //to do:  support complex numbers
                | _, _ ->
                    failwith "bad argument to + function"
            match t with
            | None ->
                res
            | Some (scmObject.Cons c) ->
                iter c res
            | _ -> failwith "bad argument list given to list function"
        | _ -> failwith "bad argument to list"
    let res = iter args None
    match res with
    | None ->
        failwith "bad result from subtraction"
    | Some (IntFloat.Int i) -> 
        scmObject.Atom (scmAtom.Int i)
    | Some (IntFloat.Float f) -> 
        scmObject.Atom (scmAtom.Float f)

//to do:  plus and times could be parameterized to use the same algorithm, pass in operator
let scmTimes (args : scmCons) (block : scmBlock option) = 
    let rec iter (args : scmCons) (res : IntFloat) =
        match args with
        | {car = Some h; cdr = t} ->
            let num = eval h block
            let res = 
                match res, num with
                | IntFloat.Float f1, scmObject.Atom (scmAtom.Float f2) ->
                    IntFloat.Float (f1 * f2)
                | IntFloat.Int i1, scmObject.Atom (scmAtom.Float f2) ->
                    IntFloat.Float ((float i1) * f2)
                | IntFloat.Int i1, scmObject.Atom (scmAtom.Int i2) ->
                    IntFloat.Int (i1 * i2)
                | IntFloat.Float f1, scmObject.Atom (scmAtom.Int i2) ->
                    IntFloat.Float (f1 * (float i2))
                //to do:  support complex numbers
                | _, _ ->
                    failwith "bad argument to * function"
            match t with
            | None ->
                res
            | Some (scmObject.Cons c) ->
                iter c res
            | _ -> failwith "bad argument list given to list function"
        | _ -> failwith "bad argument to list"
    let res = iter args (IntFloat.Int 1)
    match res with
    | IntFloat.Int i -> 
        scmObject.Atom (scmAtom.Int i)
    | IntFloat.Float f -> 
        scmObject.Atom (scmAtom.Float f)

let scmDivide (args : scmCons) (block : scmBlock option) = 
    let rec iter (args : scmCons) (res : IntFloat option) =
        match args with
        | {car = Some h; cdr = t} ->
            let num = eval h block
            let res = 
                match res, num with
                | None, scmObject.Atom (scmAtom.Float f2) ->
                    Some (IntFloat.Float f2)
                | None, scmObject.Atom (scmAtom.Int i2) ->
                    //to do:  support rational numbers
                    //Some (IntFloat.Int i2)
                    Some (IntFloat.Float (float i2))
                | Some (IntFloat.Float f1), scmObject.Atom (scmAtom.Float f2) ->
                    Some (IntFloat.Float (f1 / f2))
                | Some (IntFloat.Int i1), scmObject.Atom (scmAtom.Float f2) ->
                    Some (IntFloat.Float ((float i1) / f2))
                | Some (IntFloat.Int i1), scmObject.Atom (scmAtom.Int i2) ->
                    Some (IntFloat.Float ((float i1) / (float i2)))
                | Some (IntFloat.Float f1), scmObject.Atom (scmAtom.Int i2) ->
                    Some (IntFloat.Float (f1 / (float i2)))
                //to do:  support complex numbers
                | _, _ ->
                    failwith "bad argument to + function"
            match t with
            | None ->
                res
            | Some (scmObject.Cons c) ->
                iter c res
            | _ -> failwith "bad argument list given to list function"
        | _ -> failwith "bad argument to list"
    let res = iter args None
    match res with
    | None ->
        failwith "bad result from subtraction"
    | Some (IntFloat.Int i) -> 
        scmObject.Atom (scmAtom.Int i)
    | Some (IntFloat.Float f) -> 
        scmObject.Atom (scmAtom.Float f)

let scmSqrt (args : scmCons) (block : scmBlock option) = 
    match args.car with
    | Some c -> 
        let e = eval c block
        match e with
        | scmObject.Atom (scmAtom.Float f) ->
            let r = sqrt f
            scmObject.Atom (scmAtom.Float r)
        | scmObject.Atom (scmAtom.Int i) ->
            let r = sqrt (float i)
            scmObject.Atom (scmAtom.Float r)
        | _ -> failwith "bad argument to sqrt"
    | None -> failwith "bad argument to sqrt"

let scmCos (args : scmCons) (block : scmBlock option) = 
    match args.car with
    | Some c -> 
        let e = eval c block
        match e with
        | scmObject.Atom (scmAtom.Float f) ->
            let r = Math.Cos f
            scmObject.Atom (scmAtom.Float r)
        | scmObject.Atom (scmAtom.Int i) ->
            let r = sqrt (float i)
            scmObject.Atom (scmAtom.Float r)
        | _ -> failwith "bad argument to cos"
    | None -> failwith "bad argument to cos"

let scmSin (args : scmCons) (block : scmBlock option) = 
    match args.car with
    | Some c -> 
        let e = eval c block
        match e with
        | scmObject.Atom (scmAtom.Float f) ->
            let r = Math.Sin f
            scmObject.Atom (scmAtom.Float r)
        | scmObject.Atom (scmAtom.Int i) ->
            let r = sqrt (float i)
            scmObject.Atom (scmAtom.Float r)
        | _ -> failwith "bad argument to sin"
    | None -> failwith "bad argument to sin"

let populateEnv () =
    let env = topLevel.top
    env.add 
        (symbolTable.getSymbol "quote") 
        (scmObject.Atom (scmAtom.Primitive quote))
        true 
        stack
    env.add 
        (symbolTable.getSymbol "car") 
        (scmObject.Atom (scmAtom.Primitive car))
        true 
        stack
    env.add 
        (symbolTable.getSymbol "cdr") 
        (scmObject.Atom (scmAtom.Primitive cdr)) 
        true 
        stack
    env.add 
        (symbolTable.getSymbol "cons") 
        (scmObject.Atom (scmAtom.Primitive cons))
        true 
        stack
    env.add 
        (symbolTable.getSymbol "lambda") 
        (scmObject.Atom (scmAtom.Primitive lambda)) 
        true 
        stack
    env.add 
        (symbolTable.getSymbol "let") 
        (scmObject.Atom (scmAtom.Primitive scmLet))
        true 
        stack
    env.add 
        (symbolTable.getSymbol "let*") 
        (scmObject.Atom (scmAtom.Primitive scmLetStar))
        true
        stack
    env.add 
        (symbolTable.getSymbol "list") 
        (scmObject.Atom (scmAtom.Primitive scmList))
        true
        stack
    env.add 
        (symbolTable.getSymbol "+") 
        (scmObject.Atom (scmAtom.Primitive scmPlus))
        true
        stack
    env.add 
        (symbolTable.getSymbol "-") 
        (scmObject.Atom (scmAtom.Primitive scmMinus))
        true
        stack
    env.add 
        (symbolTable.getSymbol "*") 
        (scmObject.Atom (scmAtom.Primitive scmTimes))
        true
        stack
    env.add 
        (symbolTable.getSymbol "/") 
        (scmObject.Atom (scmAtom.Primitive scmDivide))
        true
        stack
    env.add 
        (symbolTable.getSymbol "sqrt") 
        (scmObject.Atom (scmAtom.Primitive scmSqrt))
        true
        stack
    env.add 
        (symbolTable.getSymbol "cos") 
        (scmObject.Atom (scmAtom.Primitive scmCos))
        true
        stack
    env.add 
        (symbolTable.getSymbol "sin") 
        (scmObject.Atom (scmAtom.Primitive scmSin))
        true
        stack

//to do:  this should use same code base as printResults
let showTokens (str : string) =
    let prettyPrint tokens =
        let rec iter tokens acc = 
            match tokens with 
            | [] -> acc
            | h :: t ->
                let current = 
                    match h with 
                    | scmToken.Atom a ->
                        "ATOM [" + a.ToString () + "]"
                    | scmToken.Dot ->
                        "DOT"
                    | scmToken.LeftParen ->
                        "("
                    | scmToken.RightParen -> 
                        ")"
                    | scmToken.SingleQuote ->
                        "'"
                    | scmToken.Sharp s ->
                        "SHARP [" + s.ToString () + "]"
                    | scmToken.String s ->
                        "STRING [" + s.ToString () + "]"
                    | scmToken.Comment c ->
                        "COMMENT [" + c + "]"
                    | scmToken.Whitespace w ->
                        "WHITESPACE [" + w.ToString () + "]"
                iter t (current + " " + acc)
        iter (List.rev tokens) ""
    try 
        let tokens = 
            tokenize str 
            |> scmTokens.create
        let tokens =
            tokens.tokens 
            |> prettyPrint          
        tokens
    with 
        | ex -> 
            ex.ToString()

let printResults (txtIn : TextBox) (txtOut : TextBox) =
    (fun _ -> 
        let prettyPrint tokens =
            let rec iter tokens acc = 
                match tokens with 
                | [] -> acc
                | h :: t ->
                    let current = 
                        match h with 
                        | scmToken.Atom a ->
                            "ATOM [" + a.ToString () + "]"
                        | scmToken.Dot ->
                            "DOT"
                        | scmToken.LeftParen ->
                            "("
                        | scmToken.RightParen -> 
                            ")"
                        | scmToken.SingleQuote ->
                            "'"
                        | scmToken.Sharp s ->
                            "SHARP [" + s.ToString () + "]"
                        | scmToken.String s ->
                            "STRING [" + s.ToString () + "]"
                        | scmToken.Comment c ->
                            "COMMENT [" + c + "]"
                        | scmToken.Whitespace w ->
                            "WHITESPACE [" + w.ToString () + "]"
                    iter t (current + " " + acc)
            iter (List.rev tokens) ""
        let str = txtIn.Text
        try 
            let tokens = 
                tokenize str 
                |> scmTokens.create
            let tokens =
                tokens.tokens 
                |> prettyPrint          
            txtOut.Text <- tokens
        with 
            | ex -> 
                MessageBox.Show(ex.ToString()) |> ignore)

let evalString str =
    let tokens = tokenize str |> scmTokens.create
    let heap = buildHeap tokens
    eval heap None

let rec toScheme (ob : obj) =
    match ob with 
    | :? int as i ->
        scmObject.Atom (scmAtom.Int i)
    | :? float as f ->
        scmObject.Atom (scmAtom.Float f)
    | :? (double * double) as t -> 
        let cell = scmCons.create ()
        cell.car <- Some (toScheme (fst t))
        cell.cdr <- Some (toScheme (snd t))
        let quote = scmCons.create ()
        let sym = symbolTable.getSymbol "quote"
        quote.car <- Some (scmObject.Atom sym)
        let next = scmCons.create ()
        quote.cdr <- Some (scmObject.Cons next)
        next.car <- Some (scmObject.Cons cell)
        scmObject.Cons quote
    | _ -> 
        failwith "can't convert that F# type to scheme"

let rec fromScheme obj = 
    match obj with 
    | scmObject.Atom (scmAtom.Float f) -> f :> obj
    | scmObject.Atom (scmAtom.Int i) -> i :> obj
    | scmObject.Cons c -> 
        let car = (fromScheme c.car.Value) :?> double
        let cdr = (fromScheme c.cdr.Value) :?> double
        (car, cdr) :> obj
    | _ -> failwith "can't convert from the given scheme type"

let apply (func : scmBlock) args =
    let cell = scmCons.create ()
    let func = Some (scmObject.Block func)  
    cell.car <- func
    let rec iter args (scmArgs : scmCons option) (firstCell : scmCons option) =
        match args with
        | [] -> 
            firstCell
        | h :: t -> 
            if firstCell.IsNone then
                let cell = scmCons.create ()
                cell.car <- Some h
                let cell = Some cell
                iter t cell cell
            else 
                let cell = scmCons.create ()
                let current = scmArgs.Value
                current.cdr <- Some (scmObject.Cons cell)
                cell.car <- Some h
                let cell = Some cell
                iter t cell firstCell
    let args = iter args None None
    let args = args.Value
    cell.cdr <- Some (scmObject.Cons args)
    let obj = scmObject.Cons cell
    let evaled = eval obj None
    evaled
    