open Utils
let parse = My_parser.parse

exception AssertFail
exception DivByZero

let rec desugar prog = 

  let rec desugar_helper sfexpr =  (*Helper function to patternmatch for desugar*)
    match sfexpr with
    |SUnit -> Unit
    |STrue -> True (*Match basic sugars to their expr counterparts*)
    |SFalse -> False
    |SNum n-> Num n 
    |SVar v -> Var v (* Basic sugar translation*)
    |SFun {arg; args; body} -> List.fold_right (fun (name, ty) acc -> Fun(name, ty, acc)) (arg :: args) (desugar_helper body)
    |SApp (e1, e2) -> App(desugar_helper e1, desugar_helper e2)
    |SLet {is_rec; name; args; ty; value; body} -> 
      let expr_val = if args = [] then desugar_helper value (*If theres only no arguements just evaluate value*)
        else List.fold_right (fun (arg_name, arg_ty) acc -> Fun(arg_name, arg_ty, acc)) args (desugar_helper value) in (*else collect all of the eval expressions like before*)
      let ty_function = List.fold_right (fun(_, arg_ty) acc -> FunTy(arg_ty, acc)) args ty in (*collect the types as well*)
      Let {is_rec = is_rec; name = name; ty = ty_function; value = expr_val; body = desugar_helper body}  (*Make the let express, this is how we get nested lets and let recs*)
    |SIf (e1, e2, e3) -> If(desugar_helper e1, desugar_helper e2, desugar_helper e3) (*Just desugar the expressions*)
    |SBop (op, e1, e2) -> Bop(op, desugar_helper e1, desugar_helper e2) (*Just desugar the expressions*)
    |SAssert (e) -> Assert(desugar_helper e)  in

  match prog with
  |[] -> Unit
  |{is_rec; name; args; ty; value} :: ls ->
    let expr_val = if args = [] then desugar_helper value (*If theres no arguements just perform desugar on value*)
      else List.fold_right (fun (arg_name, arg_ty) acc -> Fun(arg_name, arg_ty, acc)) args (desugar_helper value) in (*else there are multiple arguements and we need to seperate them into nested functions with  the correct exprs*)
    let ty_function = List.fold_right (fun(_, arg_ty) acc -> FunTy(arg_ty, acc)) args ty in (*same process for all the types as well*)
    Let {is_rec = is_rec; name = name; ty = ty_function; value = expr_val; body = desugar ls} (*store the nested functions in let in the let *) 




    (*Helper function for desugar to actually pattern match the SFExpr to their Expr counter parts *)


(*Type of helper functions*)

let rec look_up v ctxt =
  match ctxt with
  | [] -> Error (UnknownVar v)  (* Error case when the variable is not found *)
  | (v1, t1) :: ls ->
      if v1 = v then Ok t1 (*If the correct variable is found return it*)
      else look_up v ls (*Else keep searching*)
 
(*Type_of*)

let type_of expr =
  let rec go ctxt expr =
    match expr with  
    | Num _ -> Ok IntTy (*Basic cases*)
    | Unit -> Ok UnitTy
    | True -> Ok BoolTy
    | False -> Ok BoolTy
    | Var v -> look_up v ctxt
    | If (e1, e2, e3) -> 
        (match go ctxt e1 with
        | Ok BoolTy -> 
            (match go ctxt e2, go ctxt e3 with
            | Ok ty1, Ok ty2 when ty1 = ty2 -> Ok ty1 (*make sure that e2 and e3 are of the same type*)
            | Ok ty1, Ok ty2 -> Error (IfTyErr (ty1, ty2))  
            | Error err, _ -> Error err
            | _, Error err -> Error err)
        | Ok ty -> Error (IfCondTyErr ty)  
        | Error err -> Error err)
    | Bop (b, e1, e2) -> 
        (match b with
        | Add | Sub | Mul | Div | Mod ->
            (match go ctxt e1 with
            |Ok t1 -> if t1 <> IntTy then Error (OpTyErrL(b, IntTy, t1)) (*Raise and error depending on whether the left or right type is false*)
            else 
              (match go ctxt e2 with
              |Ok t2 -> if t2 = IntTy then Ok IntTy else Error (OpTyErrR(b, IntTy, t2))
              |Error err -> Error err) 
            |Error err -> Error err)
        | Lt | Lte | Gt | Gte | Eq | Neq ->
            (match go ctxt e1 with 
            |Ok t1 -> if t1 <> IntTy then Error (OpTyErrL(b, IntTy, t1)) 
            else
              (match go ctxt e2 with
              |Ok t2 -> if t2 = IntTy then Ok BoolTy else Error (OpTyErrR(b, IntTy, t2))
              |Error err -> Error err)
            |Error err -> Error err)
        | And | Or ->
          (match go ctxt e1 with 
          |Ok b1 -> if b1 <> BoolTy then Error (OpTyErrL(b, BoolTy, b1)) 
          else
            (match go ctxt e2 with
            |Ok b2 -> if b2 = BoolTy then Ok BoolTy else Error (OpTyErrR(b, BoolTy, b2))
            |Error err -> Error err)
          |Error err -> Error err))
    | Fun (var, ty, e) ->
        (match go ((var, ty) :: ctxt) e with (*Evaluate the expression with  the var and type added to teh context*)
        | Ok body_ty -> Ok (FunTy (ty, body_ty)) (*If it returns a valid type 2*)
        | Error err -> Error err)
    | App (e1, e2) -> 
        (match go ctxt e1, go ctxt e2 with
        | Ok (FunTy (ty_arg, ty_out)), Ok ty2 when ty_arg = ty2 -> Ok ty_out (*Make sure the expression one is a functio and that e2 and e1 are of the same type*)
        | Ok (FunTy (ty_arg, _)), Ok ty2 -> Error (FunArgTyErr (ty_arg, ty2))
        | Ok nonfun, _ -> Error (FunAppTyErr nonfun)
        | Error err, _ -> Error err
        )
    | Let { is_rec; name; ty; value; body} -> 
        if not is_rec then
          (match go ctxt value with (*Normal let is very straight forward, literally just add the variable to the context*)
          | Ok t1 when t1 = ty -> go ((name, ty) :: ctxt) body (*Evaluate the body with the new context*)
          | Ok t1 -> Error (LetTyErr (ty, t1))
          | Error err -> Error err)
        else(*For the recursive version its a bit more tricky*)
          let new_ctxt = (name, ty) :: ctxt in  (*Immediatly add the variable and its type context*)
          (match go new_ctxt value with (*Then evaluate the value with the new context*)
          | Ok t1 when t1 = ty -> go new_ctxt body (*If the recursion worked properly the new type should be the same as the origional, now check the body*)
          | Ok t1 -> Error (LetTyErr (ty, t1))
          | Error err -> Error err)
    | Assert e ->
        (match go ctxt e with (*If type bool is returned then return type unit*)
        | Ok ty -> if ty = BoolTy then Ok UnitTy else Error(AssertTyErr(ty))
        | Error err -> Error err)
    in go [] expr




(*Evaluator*)

let eval expr =
  let rec go env expr =
    match expr with
    |Unit -> VUnit (*basic eval matches*)
    |Num n -> VNum n
    |True -> VBool true
    |False -> VBool false
    |Var s ->  
      (match Env.find_opt s env with (*Check the environment for the variable*)
      |Some s -> s
      |None -> assert false)
    
    | Fun (x, _, b) -> VClos {name = None; arg = x; body = b; env = env} (*functions evaluat too unnamed VCloses*)

    |Bop(bop, expr1, expr2) ->
      (match go env expr1, go env expr2 with
      |VNum x,  VNum y ->  (*if expr1 and expr2 evaluate to valid integers, then they can be matched with bop(I'm sure i could implement floats in a similar manner))*)
        (match bop with
        |Add -> VNum (x + y) (*Now just apply the chosen operator tp the VNums*)
        |Sub -> VNum (x - y)
        |Mul -> VNum (x * y)
        |Div -> if y <> 0 then VNum (x / y) else raise DivByZero (*Make sure to check for cases where div by zero could occur*)
        |Mod -> if y <> 0 then VNum (x mod y) else raise DivByZero
        |Lt -> VBool (x < y)
        |Lte -> VBool (x <= y)
        |Gt -> VBool (x > y)
        |Gte -> VBool(x >= y)
        |Eq -> VBool (x = y)
        |Neq -> VBool (x <> y)
        |_ -> assert false
        )
      |VBool b1, VBool b2 -> (*If e1 and e2 are bools then match bop with bool operations*)
        (match bop with
        |Eq -> VBool (b1 = b2) (*return the correct boolean value.*)
        |Neq -> VBool (b1 <> b2)
        |And -> VBool (b1 && b2)
        |Or -> VBool (b1 || b2)
        |_ -> assert false)
      |_ -> assert false
      )
    |If (e1, e2, e3) -> 
      (match go env e1 with
      |VBool true -> go env e2
      |VBool false -> go env e3
      |_ -> assert false)
    |Let {is_rec = true; name = x; ty = _; value = e1; body = e2} ->
      (match go env e1 with
      |VClos {name = _; arg; body; env = closure_env} -> (*VClos allows the recursive call to carry its environment with it to be called on later*)
        let new_env = Env.add x (VClos{name = Some x; arg; body; env = closure_env}) env in (*Store VClos in the environment for the recursive call to be able to reference itself*)
        go new_env e2
      |_ -> assert false)
    |Let {is_rec = false; name = x; ty =_; value = e1; body = e2} ->
      let v = go env e1 in (*This is way more straight forward than the recursive call, just evaluate e1 then*)
      go (Env.add x v env) e2 (*Add it to the environment with X before addressing e2*)
    |App (e1, e2) ->
      (match go env e1 with
      |VClos {name = None; arg; body; env = fun_env}-> (*make sure that e1 evaluates to a non-rec "function"*)
        let v = go env e2 in (*Evaluate e2*)
        go (Env.add arg v fun_env) body (*put the value into the functions environment to apply it, pair it with arg "the name"*)
      |VClos {name = Some n; arg; body; env = fun_env} -> (*Make sure the app is a recursive function*)
        let v = go env e2 in (*evaluate e2 to apply it to the function*)
        let new_env = Env.add n (VClos {name = Some n; arg; body; env = fun_env}) fun_env in (*Store the VClos in the fun_env so the function can call itself later*)
        go (Env.add arg v new_env) body (*Evalute the body with the new env with arg and v added to it*)
      |_ -> assert false)
    |Assert e ->
      (match go env e with
      |VBool true -> VUnit (*If true return VUnit*)
      |_ -> raise AssertFail)
  in go Env.empty expr (*Establish an empty environment so I can use add commands later*)
      
let interp s =
  match parse s with
  |None -> Error ParseErr (*If the expressions is invalid then the parse has failed :( )*)
  |Some prog -> 
    let desugared = desugar prog in 
    (match type_of desugared with (*Check the typing of the expression first, if it passes the type check then it should pass the eval.*)
    |Ok _-> Ok (eval desugared)
    |Error err -> Error err)
   (*else run evaluation on the expression*)