
open Utils
let parse = My_parser.parse

let rec subst (v:value) (x: string) (e:expr) : expr = (*Instead of substitution, we can make a list of tuples to keep track of assigned variables, may be more efficient for bigger programs*)
  match e with
  |Num _-> e (*During substitution, Nums, bools and units just have to return as themselves*)
  |Var y -> 
    if x = y then 
      (match v with
      |VNum n -> Num n
      |VBool b -> if b then True else False
      |VUnit -> Unit
      |VFun (y, e) -> Fun (y, e))
    else e(*Check if the var we are substituting in matches var in e, if so return the given value*)
  |Unit -> e
  |True -> e
  |False -> e
  |App (e1, e2) ->  (*if e is an application, just substitute into both the applications expressions*)
    let e1' = subst v x e1 in 
    let e2' = subst v x e2 in
    App (e1', e2')
  |Bop (bop, e1, e2) -> (*Same case with bop as app, the actual operator shouldnt be changed*)
    let e1' = subst v x e1 in
    let e2' = subst v x e2 in
    Bop (bop, e1', e2')
  |If(e1, e2, e3) -> (*For if just substitute all 3 expressions*)
    let e1' = subst v x e1 in
    let e2' = subst v x e2 in
    let e3' = subst v x e3 in
    If(e1', e2', e3')
  |Let (y, e1, e2) ->  (*Let is a little tricky, substitute in the given value to e1. However if the var let var matches the given var*)
    let e1' = subst v x e1 in (*Then dont substitute it in for e2 because the given var body/value is being changed to the body of y*)
    if x = y 
    then Let (y, e1', e2)
    else Let (y, e1', subst v x e2)
  |Fun (y, e1) -> 
    if x = y
    then
    Fun (y, e1) (*Same case as let, if x and y match then the var is being assigned to a value, and theres no need for substitution*)
    else
    let e1' = subst v x e1 in 
    Fun (y, e1') 




let rec eval expr =
  match expr with
  |Num n -> Ok(VNum n)  
  |Var s -> Error(UnknownVar s) (*if we ever get to this point then there is an error*)
  |Unit -> Ok VUnit
  |True -> Ok(VBool true)
  |False -> Ok(VBool false)
  |App(e1, e2) -> 
    (match eval e1 with
    |Ok (VFun(param, body)) ->  (*Make sure that e1 always evaluates to a function, other wise the app is invalid*)
      (match eval e2 with
      |Ok argument ->  (*Make sure e2 returns a valid argument*)
        let new_body = subst argument param body in (*substitute the arguement into the function*)
        eval new_body (*now evaluate the substitiuted function*)
      |Error err -> Error err)
    |Ok _ -> Error(InvalidApp)
    |Error err -> Error err
    )
  |Bop(bop, expr1, expr2) ->
    (match eval expr1, eval expr2 with
    |Ok (VNum x),  Ok (VNum y) ->  (*if expr1 and expr2 evaluate to valid integers, then they can be matched with bop(I'm sure i could implement floats in a similar manner))*)
      (match bop with
      |Add -> Ok (VNum (x + y)) (*NNow just apply the chosen operator tp the VNums*)
      |Sub -> Ok (VNum (x - y))
      |Mul -> Ok (VNum (x * y))
      |Div -> Ok (VNum (x / y))
      |Mod -> Ok (VNum (x mod y))
      |Lt -> Ok (VBool (x < y))
      |Lte -> Ok (VBool (x <= y))
      |Gt -> Ok (VBool (x > y))
      |Gte -> Ok (VBool(x >= y))
      |Eq -> Ok(VBool (x = y))
      |Neq -> Ok(VBool (x <> y))
      |_ -> Error (InvalidArgs bop)
      )
    |Ok (VBool b1), Ok (VBool b2) -> (*If e1 and e2 are bools then match bop with bool operations*)
      (match bop with
      |Eq -> Ok (VBool (b1 = b2)) (*return the correct boolean value.*)
      |Neq -> Ok (VBool (b1 <> b2))
      |And -> Ok (VBool (b1 && b2))
      |Or -> Ok (VBool (b1 || b2))
      |_ -> Error (InvalidArgs bop))
    |_ -> Error(InvalidArgs bop)
    )
  |If (e1, e2, e3) -> (* If is pretty easy, *)
    (match (eval e1) with
    |Ok(VBool true) -> eval e2 (*If e1 true return the evaluation of e2*)
    |Ok(VBool false) -> eval e3(*If e1 is false return the evaluationg of e3*)
    |_ -> Error (InvalidIfCond)) (*else there must be some sort of error*)
  |Let (str, e1, e2) ->
   (match (eval e1) with
   |Ok e1' -> (*If e1 evaluates to some ok value, substitute it in to e2*)
    let e2' = subst e1' str e2 in
    eval e2' (*Return the evaluation of the e2'*)
   |Error err -> Error err
   )
  |Fun (str, expr) -> Ok(VFun(str, expr)) (*Functions just return functions*)
    
  let interp s =
    match parse s with
    |None -> Error ParseFail (*If the expressions is invalid then the parse has failed :( )*)
    |Some expr -> eval (expr)  (*else run evaluation on the expression*)
