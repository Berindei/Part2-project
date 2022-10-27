exception Unimplemented;;

exception UnexpectedError;;

type var = string;;

let fstring = Printf.sprintf;;

type typ = 
    | TUnit
    | Loli   of typ * typ
    | Tensor of typ * typ
    | Sum    of typ * typ;;

let rec printtype t = 
    match t with
    | TUnit           -> "1"
    | Loli (t1, t2)   -> fstring "(%s⊸%s)" (printtype t1) (printtype t2)
    | Tensor (t1, t2) -> fstring "(%s⊗%s)" (printtype t1) (printtype t2)
    | Sum (t1, t2)    -> fstring "(%s+%s)" (printtype t1) (printtype t2);;

type expr = 
    | EUnit
    | Var    of var
    | Lambda of var * expr
    | App    of expr * expr
    | Pair   of expr * expr
    | Unpair of var * var * expr * expr
    | Annot  of expr * typ
    | L      of expr
    | R      of expr
    | Case   of expr * var * expr * var * expr;;

let rec printexpr e =
    match e with
    | EUnit -> "()"
    | Var x -> x
    | Lambda (x, e')            -> fstring "λ%s.%s" x (printexpr e')
    | App (e1, e2)              -> fstring "(%s)(%s)" (printexpr e1) (printexpr e2)
    | Pair (e1, e2)             -> fstring "(%s, %s)" (printexpr e1) (printexpr e2)
    | Unpair (x1, x2, e1, e2)   -> fstring "let (%s,%s) = %s in %s" x1 x2 (printexpr e1) (printexpr e2)
    | Annot (e', t)             -> fstring "(%s: %s)" (printexpr e') (printtype t)
    | L e'                      -> fstring "L(%s)" (printexpr e')
    | R e'                      -> fstring "R(%s)" (printexpr e')
    | Case (e', x1, e1, x2, e2) -> fstring "case(%s, L(%s)->%s, R(%s)->%s)" (printexpr e') x1 (printexpr e1) x2 (printexpr e2);;

type usage = Used | Fresh
type state = {var: var; used: usage; typ: typ}

let mkstate v u t = {var=v; used=u; typ=t}
let fresh v t = mkstate v Fresh t

type ctx = state list

type errorinfo = string

type 'a result = Value of 'a | Error of errorinfo

type 'a t = ctx -> ('a * ctx) result

let return (x: 'a) : 'a t = fun (state: ctx) -> Value(x, state)

let bind (x: 'a t) (f: 'a -> 'b t) : 'b t = fun (state: ctx) ->
    match x state with
    | Value (y, s) -> f y s
    | Error e -> Error e
    
let (>>=) = bind

let rec empty (ctx: ctx) : bool =
    match ctx with
    | []                         -> true
    | x :: xs when x.used = Used -> empty xs
    | _                          -> false

let rec lookup: var -> state t = fun (x: var) -> fun (ctx: ctx) ->
    match ctx with
    | []                     -> Error (fstring "Variable %s not in context" x)
    | y :: ys when x = y.var -> Value (y, y :: ys)
    | y :: ys                -> (lookup x >>= (fun s -> fun ctx' -> Value(s, y :: ctx'))) ys

let rec lookup_update: var -> state t = fun (x: var) -> fun (ctx: ctx) ->
    match ctx with
    | []                     -> Error (fstring "Variable %s not in context" x)
    | y :: ys when x = y.var -> Value (y, {y with used=Used} :: ys)
    | y :: ys                -> (lookup_update x >>= (fun s -> fun ctx' -> Value(s, y :: ctx'))) ys

let rec rm l v =
    match l with
    | x :: xs when v=x -> xs
    | x :: xs          -> x :: (rm xs v)
    | []               -> raise UnexpectedError

let rec find l v = 
    match l with
    | x :: xs when v=x -> true
    | x :: xs          -> find xs v
    | []               -> false

let rec same (ctx1: ctx) (ctx2: ctx) : bool = 
    match ctx1, ctx2 with
    | s :: ss, _ -> if find ctx2 s then same ss (rm ctx2 s)
                                    else false
    | [], []     -> true
    | [], _      -> false



let rec check (e: expr) (t: typ) : unit t = 
    match e, t with
    | EUnit, TUnit -> fun (ctx: ctx) -> if empty ctx then Value ((), ctx)
                                    else Error "Non empty context for unit expression"
    | Lambda (x, e'), Loli (t1, t2) -> fun (ctx: ctx) -> (fresh x t1 :: ctx) |> ((check e' t2) >>= (fun (_: unit) -> fun (ctx': ctx) -> (match lookup x ctx' with 
                                                                                                                                            | Value(s, _) -> (match s.used with
                                                                                                                                                            | Used -> Value((), ctx)
                                                                                                                                                            | Fresh -> Error (fstring "Variable %s not used in function %s" x (printexpr e)))
                                                                                                                                            | Error _     -> raise UnexpectedError))) 
    | Pair (e1, e2), Tensor (t1, t2) -> (check e1 t1) >>= (fun () -> check e2 t2)
    | Unpair (x1, x2, e1, e2), _ -> (infer e1) >>= (fun (Tensor(t1, t2): typ) ->
                                        fun (ctx: ctx) -> ((fresh x1 t1) :: (fresh x2 t2) :: ctx) |> ((check e2 t) >>= (fun () ->
                                        fun (ctx': ctx) -> (match (lookup x1 ctx'), (lookup x2 ctx') with
                                                            | Value (s1, _), Value (s2, _) -> (match s1.used, s2. used with
                                                                                            | Used, Used -> Value ((), ctx)
                                                                                            | Fresh, Used -> Error (fstring "Variable %s not used in the body of %s" x1 (printexpr e))
                                                                                            | Used, Fresh -> Error (fstring "Variable %s not used in the body of %s" x2 (printexpr e))
                                                                                            | Fresh, Fresh -> Error (fstring "Variables %s and %s not used in the body of %s" x1 x2 (printexpr e)))
                                                            | _ -> raise UnexpectedError))))
    | L(e'), Sum(t1, t2) -> check e' t1
    | R(e'), Sum(t1, t2) -> check e' t2
    | Case (e', x1, e1, x2, e2), _ -> (infer e') >>= (fun (Sum(t1, t2):typ) -> fun (ctx: ctx) ->
                                        let r1 = (fresh x1 t1 :: ctx) |> check e1 t in
                                        let r2 = (fresh x2 t2 :: ctx) |> check e2 t in
                                        match r1, r2 with
                                        | Value ((), ctx1), Value ((), ctx2) -> (match (lookup x1 ctx1), (lookup x2 ctx2) with
                                                                                | Value (s1, _), Value (s2, _) -> (match s1.used, s2.used with
                                                                                                                    | Used, Used -> if same (rm ctx1 s1) (rm ctx2 s2) then Value((), ctx)
                                                                                                                                                                    else Error (fstring "Different resulting contexts for the cases of %s" (printexpr e))
                                                                                                                    | Fresh, Used -> Error (fstring "Variable %s not used in L case of %s" x1 (printexpr e))
                                                                                                                    | Used, Fresh -> Error (fstring "Variable %s not used in R case of %s" x2 (printexpr e))
                                                                                                                    | Fresh, Fresh -> Error (fstring "Variables %s and %s not used in L and R cases of %s" x1 x2 (printexpr e)))
                                                                                | _ -> raise UnexpectedError)
                                        | Error e, _ -> Error e
                                        | _, Error e -> Error e)
                                        
    | _, _ -> ((infer e) >>= (fun t'-> fun ctx' -> if t=t' then Value ((), ctx') else Error "Can't typecheck"))
and infer (e: expr) : typ t =
    match e with 
    | Var x -> (lookup_update x) >>= (fun (y:state) -> fun (ctx: ctx) -> match y.used with
                                                                            | Fresh -> Value (y.typ, ctx)
                                                                            | Used  -> Error (fstring "Multiple usages of variable %s in linear context" x))
    | Annot (e', t) -> (check e' t) >>= (fun (_: unit) -> return t)
    | App (e1, e2) -> (infer e1) >>= (fun (Loli(t1, t2): typ) -> 
                        check e2 t1 >>= (fun (_: unit) -> return t2))
    | _ -> raise Unimplemented

let typecheck (e: expr): typ t = 
    match e with
    | EUnit          -> (check e TUnit) >>= (fun (_: unit) -> return TUnit)
    | Var x          -> infer e
    | Annot (e', t)  -> infer e
    | App (e1, e2)   -> infer e
    | _ -> raise Unimplemented

