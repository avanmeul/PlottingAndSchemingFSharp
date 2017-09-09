module PlottingAndScheming.Scheme

open PlottingAndScheming.xml
open System
open System.Diagnostics
open System.Text.RegularExpressions
open System.Windows
open System.Windows.Controls
open System.Xml.Linq

(*
Milestone:  factorial working on the morning of 8 Sep 17 Fr before work.
*)

type scmToken =
    | LeftParen
    | RightParen
    | SingleQuote
    | Comment of string    
    | Symbol of string
    //| Complex of complex
    | Float of float
    | Int of int
    | Whitespace of string
    | Dot
    | String of string
    | Bool of char
    //| Sharp of string //to do
    //| BlockComment of string

type regexParsers () =
    let regRightParen = new Regex "^\).*"
    let regComment = new Regex "^;([^\r\n]*)[\r\n]?([\S\s]*)?"
    do ()
    member x.comment = regComment
    member x.rightParen = regRightParen
    
let regexParsers = regexParsers ()
          
let parseLeftParen (str : string) =
    if str.StartsWith "(" then Some scmToken.LeftParen, str.[1..]
    else None, str
        
let parseRightParen (str : string) =
    if str.StartsWith ")" then Some scmToken.RightParen, str.[1..]
    else None, str

let parseSingleQuote (str : string) =
    if str.StartsWith "'" then Some scmToken.SingleQuote, str.[1..]
    else None, str

let parseComment str =
    let reg = regexParsers.comment
    let m = reg.Match str
    if m.Success then Some <| scmToken.Comment m.Groups.[1].Value, m.Groups.[2].Value
    else None, str  

let symbolChars = 
    ['a'..'z'] @ ['A'..'Z'] @ ['0'..'9'] @
    ['~'; '!'; '@'; '.'; '_'; '-'; '+'; '*'; '&'; '$'; '^'; '%'; '?'; '='; ':'; '<'; '>'; '\\'; '/']
let whitespaceChars = ['\n'; '\r'; ' '; '\t']

let symbolCharSet = Set.ofList symbolChars
let whitespaceCharSet = Set.ofList whitespaceChars

let parseSymbol (str : string) =
    let len = str.Length
    let rec iter i hi =
        if i < len && Set.contains str.[i] symbolCharSet then
            iter (i + 1) i
        else
            hi
    let hi = iter 0 -1
    if hi = -1 || (hi = 0 && str.[0] = '.') then
        (None, str)
    else
        let cdr = 
            if hi = str.Length then
                ""
            else 
                str.Substring(hi + 1)
        (Some (scmToken.Symbol (str.Substring (0, hi + 1))), cdr)

let parseDot (str : string) = 
    if str.[0] = '.' then
        (Some scmToken.Dot, str.Substring(1))
    else
        (None, str)
    
let parseBool (str : string) =
    let len = str.Length
    if len >= 2 && 
        str.[0] = '#' && 
        (str.[1] = 't' || str.[1] = 'f') && 
        (len < 3 || (not (List.contains str.[2] symbolChars))) then
        Some (scmToken.Bool str.[1]), str.Substring(2)
    else
        None, str

//let scmTrue = scmObject.Atom (scmAtom.Sharp "t")
//let scmFalse = scmObject.Atom (scmAtom.Sharp "f")

//let parseTrue (str : string) = 
//    if str.StartsWith "#t" then
//        (Some scmTrue, str.Sub(2))
//    else
//        None, str

let parseWhitespace (str : string) =
    let len = str.Length
    let rec iter i hi =
        if i < len && Set.contains str.[i] whitespaceCharSet then
            iter (i + 1) i
        else
            hi
    let hi = iter 0 -1
    if hi = -1 then
        (None, str)
    else
        let cdr = 
            if hi = str.Length then
                ""
            else 
                str.Substring(hi + 1)
        (Some (scmToken.Whitespace (str.Substring (0, hi + 1))), cdr)

let parseString (str : string) =
    if str.[0] = '\"'  then 
        let max = str.Length
        let rec iter i =
            if i >= max then
                failwith ("unterminated string:  " + str)
            else 
                if str.[i] = '\"' then
                    Some (scmToken.String (str.Substring (1, i - 1))), str.Substring (i + 1)
                else
                    iter (i + 1) 
        iter 1
    else 
        (None, str)

let regInt = new Regex "^[+|-]{0,1}[0-9]+"
let regFloat = new Regex "^([+|-]{0,1}[0-9]+[.]{1}[0-9]+)([e]{1}[+|-]{0,1}[0-9]+){0,1}"

let parseFloat str = 
    let candidate = regFloat.Match str
    if candidate.Success then
        let cdr = 
            if candidate.Length = str.Length then
                ""
            else
                str.Substring(candidate.Length)
        let s, v = Double.TryParse candidate.Value
        let adjacentSymbol = parseSymbol cdr
        match fst adjacentSymbol with
        | Some (scmToken.Symbol x) -> 
            (Some (scmToken.Symbol (candidate.Value + x)), snd adjacentSymbol)
        | _ ->
            (Some (scmToken.Float v), cdr)
    else
        (None, str)

let parseInt str = 
    let candidate = regInt.Match str
    if candidate.Success then
        let cdr = 
            if candidate.Length = str.Length then
                ""
            else
                str.Substring(candidate.Length)
        let s, v = Int32.TryParse candidate.Value
        let adjacentSymbol = parseSymbol cdr
        match fst adjacentSymbol with
        | Some (scmToken.Symbol x) -> 
            (Some (scmToken.Symbol (candidate.Value + x)), snd adjacentSymbol)
        | _ ->
            (Some (scmToken.Int v), cdr)
    else
        (None, str)

let parsers = [
    parseString;
    parseComment; 
    parseSingleQuote; 
    parseWhitespace
    parseLeftParen; 
    parseRightParen; 
    parseFloat; 
    parseInt; 
    parseSymbol;
    parseDot;
    parseBool ]

let parse str =
    let rec iter str parseList tokens =
        match str with
        | "" -> List.rev tokens
        | _ -> 
            match parseList with
            | [] -> failwith ("parse failure:  " + str)
            | h::t -> 
                let attempt = h str
                match attempt with
                | None, cdr -> 
                    iter cdr t tokens
                | Some x, cdr -> 
                    iter cdr parsers (x :: tokens)
    iter str parsers []

let rec untokenize (lst : scmToken list) =
    match lst with
    | [] -> ""
    | h::t ->
        match h with 
        | scmToken.LeftParen -> 
            "(" + (untokenize t)
        | scmToken.RightParen ->
            ")" + (untokenize t)
        | scmToken.SingleQuote ->
            "'" + (untokenize t)
        | scmToken.Comment x ->
            ";" + x + (untokenize t)
        | scmToken.Float x ->
            x.ToString() + (untokenize t)
        | scmToken.Int x ->
            x.ToString() + (untokenize t)
        | scmToken.Symbol x | scmToken.String x | scmToken.Whitespace x ->
            x + (untokenize t)
        | scmToken.Dot ->
            "." + (untokenize t)
        | scmToken.Bool x ->
            "#" + x.ToString () + (untokenize t)

let tokenListToString (lst : scmToken list) =
    let rec iter lst =
        match lst with
        | [] -> ""
        | h::t ->
            match h with 
            | scmToken.LeftParen -> 
                "( " + (iter t)
            | scmToken.RightParen ->
                ") " + (iter t)
            | scmToken.SingleQuote ->
                "' " + (iter t)
            | scmToken.Whitespace x ->
                "WhiteSpace[" + x + "] " + (iter t)
            | scmToken.Comment x ->
                "Comment[" + x + "] " + (iter t)
            | scmToken.Float x ->
                "Float[" + x.ToString() + "] " + (iter t)
            | scmToken.Int x ->
                "Int[" + x.ToString() + "] " + (iter t)
            | scmToken.Symbol x ->
                "Symbol[" + x + "] " + (iter t)
            | scmToken.Dot ->
                ". " + iter t
            | scmToken.String x ->
                "String[" + x + "] " + (iter t)
            | scmToken.Bool x ->
                "Bool[" + x.ToString () + "] " + (iter t)
    iter lst

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
    member x.toXml () = 
        //to do:  block (call printheap), parent
        let xml = new XElement (xname "scmBlock")
        xml.Add (new XElement ((xname "block"), scmObject.printHeap (scmObject.Cons x.block)))   
        let xmlBlockType =
            match x.blockType with
            | scmBlockType.Lambda -> "lambda"
            | scmBlockType.Let -> "let"
        xml.Add (new XElement ((xname "blockType"), xmlBlockType))        
        let xmlEnv = 
            match x.env with
            | None -> new XElement (xname "env")
            | Some x -> x.toXml ()
        xml.Add (new XElement ((xname "env"), xmlEnv))
        let xmlParent =
            match x.parent with
            | None -> new XElement (xname "parent")
            | Some x -> x.toXml ()
        xml.Add (new XElement ((xname "parent"), xmlParent))            
        xml
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
    static member printHeap (heap : scmObject) : string =
        let rec iter heap =
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
                iter t
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
                            s <- s + (iter h) + " . " + (iter t) + ")"
                            keepGoing <- false
                        | scmObject.Block b ->
                            failwith "not implemented yet"
                        | scmObject.Cons c -> 
                            s <- s + (iter h) + " " 
                            cell <- c
                        | scmObject.Thunk t ->
                            let t = t.value
                            s <- s + (iter t)
                    | Some h, None ->
                        s <- s + (iter h) + ")"
                        keepGoing <- false
                    | None, _ ->
                        failwith "this should never happen:  bad list"
                    firstCell <- false
                s
        iter heap
and scmCons = {
    mutable car : scmObject option;
    mutable cdr : scmObject option; } with
    static member create () = 
        { car = None; cdr = None; }
and bindingNew = {
    symbol : scmAtom;
    mutable thunk : scmThunk; } with
    member x.toXml () = 
        let xml = new XElement ((xname "symbol"), x.symbol)
        xml
and scmEnv = 
    { mutable bindings : bindingNew list } with
    static member create () : scmEnv = { bindings = [] }
    member x.print () =
        x.bindings.Length
    member x.toXml () = //to do
        let root = new XElement (xname "env")
        let rec iter (lst : bindingNew list) =
            match lst with
            | [] -> root
            | h::t -> 
                let sym = 
                    match h.symbol with
                    | scmAtom.Symbol x | scmAtom.String x -> x
                    | scmAtom.Complex x -> x.ToString ()
                    | scmAtom.Int x -> x.ToString ()
                    | scmAtom.Float x -> x.ToString ()
                    | scmAtom.Primitive x -> x.ToString ()
                    | scmAtom.Sharp x -> x.ToString ()                  
                let el = new XElement ((xname "symbol"), sym)
                root.Add el
                iter t
        let xml = iter x.bindings
        xml
    member x.add symbol value evaled (stack : scmStack) = 
        let parent = stack.head
        let thunk = scmThunk.create parent value
        let bindingNew = { symbol = symbol; thunk = thunk }
        x.bindings <- bindingNew :: x.bindings
        ()
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
    member x.toXml () = //to do
        ()

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
    member x.toXml () = 
        x.top.bindings.Length
    member x.print () = //to do:  first list how many elements, then display them and levels
        x.top.print ()
        
let stack = scmStack.create ()

let symbolTable = symTable.create
let topLevel = globalEnv.create (scmEnv.create ())
let nil = scmCons.create ()

let scmTrue = scmObject.Atom (scmAtom.Sharp "t")
let scmFalse = scmObject.Atom (scmAtom.Sharp "f")

let buildHeap (tokens : scmTokens) =
    //to do:  strip comments and whitespace
    let rec recur () = 
        let rec getToken () =
            let tok = tokens.get
            match tok with 
            | scmToken.Comment _ 
            | scmToken.Whitespace _ ->
                getToken ()
            | _ -> tok
        let rec peekToken () =
            let tok = tokens.peek
            match tok with 
            | scmToken.Comment _ | scmToken.Whitespace _ ->
                tokens.get |> ignore
                peekToken ()
            | _ -> tok
        let tok = getToken ()
        match tok with
        | scmToken.String s ->
            scmObject.Atom (scmAtom.String s)
        | scmToken.Float f -> scmObject.Atom (scmAtom.Float f)
        | scmToken.Int i -> scmObject.Atom (scmAtom.Int i)
        | scmToken.Symbol s -> scmObject.Atom (scmAtom.Symbol s)
//        | scmToken.Sharp s ->
//            scmObject.Atom (scmAtom.Sharp s)
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
        | scmToken.Bool b -> 
            if b = 't' then scmTrue else scmFalse
        | _ -> 
            failwith "not implemented yet"
    recur ()



let rec eval 
    (heap : scmObject) 
    (block : scmBlock option) =
    match heap with
    | scmObject.Atom a -> 
        //atoms are self-evaluating, except for symbols (which get looked up)
        match a with
        | scmAtom.Symbol s ->
                if s = "x" || s = "y" then
                    let foo = 3
                    ()
                let res = topLevel.tryFind s block
//                let xmlBlock =
//                    match block with
//                    | None -> ""
//                    | Some x -> (x.toXml ()).ToString ()
//                Debug.WriteLine xmlBlock
                match res with 
                | None ->
                    //to do:  check parent environment
                    Debug.WriteLine ("lookup failure on symbol " + s)
                    topLevel.toXml () |> ignore                               
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
                                    |> ignore
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
                                    |> ignore
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
                |> ignore
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
                |> ignore
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

let scmDefine (args : scmCons) (block : scmBlock option) = 
    match args.car with
    | Some ((scmObject.Atom (scmAtom.Symbol s)) as sym) ->
        let arg = args.cdr
        match args.cdr with
        | Some (scmObject.Cons x) ->
            let arg = x.car
            match arg with
            | Some x -> 
                let thunk = scmThunk.create None x
                let env = topLevel.top
                env.add 
                    (symbolTable.getSymbol s)
                    x
                    true
                    stack
                sym
            | _ -> failwith "illegal target of a definition"
        | _ -> failwith "illegal target of a definition"
    | _ ->
        failwith "expected identifier as first arg to define"

let inline CAR (arg : scmCons) = arg.car
let inline CDR (arg : scmCons) = arg.cdr
let MaybeCDR (arg : scmObject option) =
    match arg with
    | Some (scmObject.Cons x) -> x.cdr
    | _ -> None
let CADR (arg : scmCons) = 
    match arg.cdr with
    | Some (scmObject.Cons x) -> x.car
    | _ -> None
let CADDR (arg : scmCons) =
    match arg.cdr |> MaybeCDR with
    | Some (scmObject.Cons x) -> x.car
    | _ -> None

//note:  if doesn't verify arity!
let scmIf (args : scmCons) (block : scmBlock option) =
    match args.car with
    | Some b ->
        let e = eval b block
        match e with
        | scmObject.Atom (scmAtom.Sharp b) ->
            match b with
            | "t" ->
                match CADR args with
                | Some x -> eval x block
                | None -> failwith "bad then clause in if"
            | "f" ->
                match CADDR args with
                | Some x -> eval x block
                | None -> failwith "bad else clause in if"
            | _ -> failwith "bad boolean argument to if"
        | _ -> failwith "non-Boolean predicate in if"
    | _ -> failwith "non-Boolean predicate in if"

let scmZero (args : scmCons) (block : scmBlock option) =
    match args.car with
    | Some b ->
        let e = eval b block
        match e with
        | scmObject.Atom (scmAtom.Float f) -> 
            if f = 0.0 then scmObject.Atom (scmAtom.Sharp "t")
            else scmObject.Atom (scmAtom.Sharp "f")
        | scmObject.Atom (scmAtom.Int i) ->
            if i = 0 then scmObject.Atom (scmAtom.Sharp "t")
            else scmObject.Atom (scmAtom.Sharp "f")
        | _ -> failwith "zero? only implemented for ints and floats"
    | _ -> failwith "bad argument to zero?"

//refactor to get rid of option type for scheme objects; instead add scmObject.Null?

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
    env.add 
        (symbolTable.getSymbol "define") 
        (scmObject.Atom (scmAtom.Primitive scmDefine))
        true
        stack
    env.add 
        (symbolTable.getSymbol "#t") 
        scmTrue
        true
        stack
    env.add 
        (symbolTable.getSymbol "#f") 
        scmTrue
        true
        stack
    env.add 
        (symbolTable.getSymbol "if") 
        (scmObject.Atom (scmAtom.Primitive scmIf))
        true
        stack
    env.add 
        (symbolTable.getSymbol "zero?") 
        (scmObject.Atom (scmAtom.Primitive scmZero))
        true
        stack

let printResults (txtIn : TextBox) (txtOut : TextBox) =
    (fun _ -> 
        let prettyPrint tokens =
            let rec iter tokens acc = 
                match tokens with 
                | [] -> acc
                | h :: t ->
                    let current = 
                        match h with 
//                        | scmToken.Atom a ->
//                            "ATOM [" + a.ToString () + "]"
                        | scmToken.Float f -> 
                            "Float[" + f.ToString() + "]"
                        | scmToken.Int i ->
                            "Int[" + i.ToString() + "]"
                        | scmToken.Symbol s ->
                            "Symbol[" + s + "]"
                        | scmToken.Dot ->
                            "DOT"
                        | scmToken.LeftParen ->
                            "("
                        | scmToken.RightParen -> 
                            ")"
                        | scmToken.SingleQuote ->
                            "'"
//                        | scmToken.Sharp s ->
//                            "SHARP [" + s.ToString () + "]"
                        | scmToken.String s ->
                            "STRING [" + s.ToString () + "]"
                        | scmToken.Comment c ->
                            "COMMENT [" + c + "]"
                        | scmToken.Whitespace w ->
                            "WHITESPACE [" + w.ToString () + "]"
                        | scmToken.Bool x -> 
                            "Bool [" + x.ToString () + "]"
                    iter t (current + " " + acc)
            iter (List.rev tokens) ""
        let str = txtIn.Text
        try 
            let tokens = 
                parse str 
                |> scmTokens.create
            let tokens =
                tokens.tokens 
                |> prettyPrint          
            txtOut.Text <- tokens
        with 
            | ex -> 
                MessageBox.Show(ex.ToString()) |> ignore)

let evalString str =
    let tokens = parse str |> scmTokens.create
    let heap = buildHeap tokens
    eval heap None

let rec fromScheme obj = 
    match obj with 
    | scmObject.Atom (scmAtom.Float f) -> f :> obj
    | scmObject.Atom (scmAtom.Int i) -> i :> obj
    | scmObject.Cons c -> 
        let car = (fromScheme c.car.Value) :?> double
        let cdr = (fromScheme c.cdr.Value) :?> double
        (car, cdr) :> obj
    | _ -> failwith "can't convert from the given scheme type"

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

let printHeap heap = scmObject.printHeap heap