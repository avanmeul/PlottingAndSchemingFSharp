module lambda

//url:  http://fsharpcode.blogspot.com/2009/08/lambda-calculus-normal-order-reducer.html 
// This implementation supports evaluation of anonymous recursion through Y Combinator (ex. Y Factorial)

type exp = | Var of string
           | Lambda of string * exp
           | Apply of exp * exp

let rec subst x v a =
  match a with 
  | Var y -> 
        if x = y then v else a
  | Lambda(y, a') ->
        if x = y then a else Lambda(y, subst x v a')
  | Apply(a', a'') ->
        Apply(subst x v a', subst x v a'')

let rec reduce e =
    let rec reduce' e = 
        match e with
        | Var _ -> e
        | Lambda (s, e') -> Lambda(s, reduce' e')
        | Apply(e1, e2) ->
           match e1 with
           | Lambda(s, e3) -> subst s e2 e3
           | _ -> Apply(reduce' e1, reduce' e2)
    reduce' e
    
let rec loop f x =
    let x' = f x
    if x = x' then x' else loop f x'

let normalOrderReducer = loop reduce 

