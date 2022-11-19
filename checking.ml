exception Unimplemented

exception UnexpectedError

type var = string

let fstring = Printf.sprintf

type typ = 
    | TVar   of var
    | LUnit
    | Loli   of typ * typ
    | Tensor of typ * typ
    | LSum   of typ * typ
    | F      of typ
    | Evt    of typ
    | IUnit
    | Arrow  of typ * typ
    | Prod   of typ * typ
    | ISum   of typ * typ
    | G      of typ

let rec printtype t = 
    match t with
    | TVar x          -> x
    | LUnit           -> "I"
    | Loli (t1, t2)   -> fstring "(%s⊸%s)" (printtype t1) (printtype t2)
    | Tensor (t1, t2) -> fstring "(%s⊗%s)" (printtype t1) (printtype t2)
    | LSum (t1, t2)   -> fstring "(%s⊕%s)" (printtype t1) (printtype t2)
    | F t'            -> fstring "F(%s)" (printtype t')
    | Evt t'          -> fstring "♢(%s)" (printtype t')
    | IUnit           -> "1"
    | Arrow (t1, t2)  -> fstring "(%s→%s)" (printtype t1) (printtype t2)
    | Prod (t1, t2)   -> fstring "(%s*%s)" (printtype t1) (printtype t2)
    | ISum (t1, t2)   -> fstring "(%s+%s)" (printtype t1) (printtype t2)
    | G t'            -> fstring "G(%s)" (printtype t')
    

type expr = 
    | EUnit
    | Var     of var
    | Lambda  of var * expr
    | App     of expr * expr
    | Pair    of expr * expr
    | Unpair  of var * var * expr * expr
    | Annot   of expr * typ
    | L       of expr
    | R       of expr
    | Case    of expr * var * expr * var * expr
    | Proj1   of expr
    | Proj2   of expr
    | EF      of expr
    | EG      of expr
    | Run     of expr
    | LetF    of var * expr * expr
    | EEvt    of expr
    | LetEvt  of var * expr * expr
    | Let     of var * expr * expr
    | Select  of var * var * expr * expr * expr * expr

let rec printexpr e =
    match e with
    | EUnit -> "()"
    | Var x -> x
    | Lambda (x, e')                  -> fstring "λ%s.%s" x (printexpr e')
    | App (e1, e2)                    -> fstring "(%s)(%s)" (printexpr e1) (printexpr e2)
    | Pair (e1, e2)                   -> fstring "(%s, %s)" (printexpr e1) (printexpr e2)
    | Unpair (x1, x2, e1, e2)         -> fstring "let (%s,%s) = %s in %s" x1 x2 (printexpr e1) (printexpr e2)
    | Annot (e', t)                   -> fstring "(%s: %s)" (printexpr e') (printtype t)
    | L e'                            -> fstring "L(%s)" (printexpr e')
    | R e'                            -> fstring "R(%s)" (printexpr e')
    | Case (e', x1, e1, x2, e2)       -> fstring "case(%s, L(%s)->%s, R(%s)->%s)" (printexpr e') x1 (printexpr e1) x2 (printexpr e2)
    | Proj1 e'                        -> fstring "π1(%s)" (printexpr e')
    | Proj2 e'                        -> fstring "π2(%s)" (printexpr e')
    | EF e'                           -> fstring "F(%s)" (printexpr e')
    | EG e'                           -> fstring "G(%s)" (printexpr e')
    | Run e'                          -> fstring "run(%s)" (printexpr e')
    | LetF (x, e1, e2)                -> fstring "let F(%s) = %s in %s" x (printexpr e1) (printexpr e2)
    | EEvt e'                         -> fstring "evt(%s)" (printexpr e')
    | LetEvt (x, e1, e2)              -> fstring "let evt(%s) = %s in %s" x (printexpr e1) (printexpr e2)
    | Let (x, e1, e2)                 -> fstring "let %s = %s in %s" x (printexpr e1) (printexpr e2)
    | Select (x1, x2, e1, e2, e3, e4) -> fstring "(from {%s←%s; %s←%s} select %s→%s | %s→%s)" x1 (printexpr e1) x2 (printexpr e2) x1 (printexpr e3) x2 (printexpr e4)

type usage = Used | Fresh | Inf
type incl(*ude*) = Use | Ignore

type state = {var: var; used: usage; typ: typ; incl: incl; delay: int}

let mkstate v u t i d = {var=v; used=u; typ=t; incl=i; delay=d}

let used v t :state= mkstate v Used t Use 0
let fresh v t = mkstate v Fresh t Use 0
let int v t = mkstate v Inf t Use 0

let delayed v t d = {(fresh v t) with delay = d}

type ctx = state list

let printusage u = match u with Fresh->"1"| Used->"0" | Inf->"∞"

let printstate s =
    let printinner =
        match s.used with
        | Inf | _ when s.delay=0 -> fstring "%s^%s: %s" s.var (printusage s.used) (printtype s.typ)
        | Used | Fresh -> fstring "%s^%s: %s[%d]" s.var (printusage s.used) (printtype s.typ) s.delay
    in let i = printinner in
    match s.incl with
    | Use -> i
    | Ignore -> fstring "#%s#" i 

let printctx c =
    let rec loop c = 
        match c with
        | []    -> ""
        | [x]     -> printstate x
        | x::xs -> fstring "%s; %s" (printstate x) (loop xs)
    in fstring "Γ = [%s]" (loop c)

type errorinfo = string

type 'a result = Value of 'a | Error of errorinfo

type 'a t = ctx -> ('a * ctx) result

let return (x: 'a) : 'a t = fun (state: ctx) -> Value(x, state)

let error (x: errorinfo) : 'a t = fun (_: ctx) -> Error x

let bind (x: 'a t) (f: 'a -> 'b t) : 'b t = fun (state: ctx) ->
    match x state with
    | Value (y, s) -> f y s
    | Error e -> Error e
    
let (>>=) = bind

let (>>) f1 f2 = f1 >>= (fun () -> f2)

let (>>>) f1 f2 = f1 >>= (fun x -> f2 x)

let (let*) = bind

let get: ctx t = fun (ctx: ctx) -> Value(ctx, ctx)

let set: ctx -> unit t = fun (nctx: ctx) -> fun (ctx: ctx) -> Value ((), nctx)

let rec lookup: var -> state t = fun (x: var) -> fun (ctx: ctx) ->
    match ctx with
    | []                     -> Error (fstring "Variable %s not in context" x)
    | y :: ys when x = y.var && y.incl = Ignore -> Error (fstring "Variable %s not available in this context" x)
    | y :: ys when x = y.var -> Value (y, y :: ys)
    | y :: ys                -> (lookup x >>= (fun s -> fun ctx' -> Value(s, y :: ctx'))) ys

let rec lookup_update: var -> state t = fun (x: var) -> fun (ctx: ctx) ->
    match ctx with
    | []                     -> Error (fstring "Variable %s not in context" x)
    | y :: ys when x = y.var && y.incl = Ignore -> Error (fstring "Variable %s not available in this context" x)
    | y :: ys when x = y.var && y.used = Inf -> Value (y, ctx)
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

let withvar: state -> 'a t -> 'a t = fun (x: state) -> fun (m: 'a t) ->
                                    let* ctx = get in print_endline (fstring "Withvar on %s" (printctx (x::ctx)));
                                    let* r = ((set (x::ctx)) >> m) in
                                    let* ctx' = get in
                                    let* s = lookup x.var in
                                    match s.used with
                                    | Inf | Used -> set (rm ctx' s) >> return r
                                    | Fresh -> error (fstring "Unused linear variable %s" x.var)

(*let rec withvars: state list -> 'a t -> 'a t = fun xs -> fun m ->
                                               match xs with
                                               | []    -> m
                                               | x::xs -> withvar x (withvars xs m)*)

let withvars: state list -> 'a t -> 'a t = fun xs -> fun m ->
                                           let* ctx = get in
                                           let* r = ((set (xs@ctx)) >> m) in
                                           let* ctx' = get in
                                           let rec checker l c=
                                               match l with
                                               | []    -> set c >> return r
                                               | x::xs -> let* s = lookup x.var in
                                                          match s.used with
                                                          | Inf | Used -> checker xs (rm c s)
                                                          | Fresh -> error (fstring "Unused linear variable %s" s.var)
                                           in checker xs ctx'

let empty: ctx -> unit t = fun ctx ->
    match List.find_opt (fun s -> if s.used=Fresh && s.incl=Use then true else false) ctx with
    | Some s -> error (fstring "Unused variable %s in linear context" s.var)
    | None -> return ()

let unexpectedform s1 s2 = fstring "Expected something with the type of the form %s, got %s" s1 (printtype s2)

let plsProd: typ -> (typ*typ) t = fun t ->
    match t with
    | Prod (t1, t2) -> return (t1, t2)
    | _ -> error (unexpectedform "α*β" t)

let plsTensor: typ -> (typ*typ) t = fun t ->
    match t with
    | Tensor (t1, t2) -> return (t1, t2)
    | _ -> error (unexpectedform "α⊗β" t)

let plsArrow: typ -> (typ*typ) t = fun t ->
    match t with
    | Arrow (t1, t2) -> return (t1, t2)
    | _ -> error (unexpectedform "α→β" t)

let plsLoli: typ -> (typ*typ) t = fun t ->
    match t with
    | Loli (t1, t2) -> return (t1, t2)
    | _ -> error (unexpectedform "α⊸β" t)

let plsLSum: typ -> (typ*typ) t = fun t ->
    match t with
    | LSum (t1, t2) -> return (t1, t2)
    | _ -> error (unexpectedform "α⊕β" t)

let plsISum: typ -> (typ*typ) t = fun t ->
    match t with
    | ISum (t1, t2) -> return (t1, t2)
    | _ -> error (unexpectedform "α+β" t)

let plsF: typ -> typ t = fun t ->
    match t with
    | F(t) -> return t
    | _ -> error (unexpectedform "F(α)" t)

let plsG: typ -> typ t = fun t ->
    match t with
    | G(t) -> return t
    | _ -> error (unexpectedform "G(α)" t)

let plsEvt t = 
    match t with
    | Evt(t) -> return t
    | _ -> error (unexpectedform "♢(α)" t)

let lim: ctx -> ctx t = fun ctx ->
    return (List.map (fun s -> if s.used!=Inf then {s with incl=Ignore} else s) ctx)

type ent = Lin | Int

let printent e = match e with Lin->"linear" | Int->"intuitionistic"

let rec check (e: expr) (t: typ) (ent: ent) : unit t = print_endline (fstring "Checking %s against %s in %s" (printexpr e) (printtype t) (printent ent));
    match e, t, ent with
    | EUnit, LUnit, Lin -> return ()
    | EUnit, IUnit, Int -> return ()
    | Lambda (x, e'), Loli (t1, t2), Lin -> withvar (fresh x t1) (check e' t2 Lin)
    | Lambda (x, e'), Arrow (t1, t2), Int -> withvar (int x t1) (check e' t2 Int)
    | Pair (e1, e2), Tensor (t1, t2), Lin -> check e1 t1 Lin >> check e2 t2 Lin
    | Pair (e1, e2), Prod (t1, t2), Int -> check e1 t1 Int >> check e2 t2 Int
    | Unpair (x1, x2, e1, e2), _, Lin -> let* t1, t2 = infer e1 Lin >>> plsTensor in
                                    withvars ([fresh x1 t1; fresh x2 t2]) (check e2 t Lin)
    | L(e'), LSum(t1, t2), Lin -> check e' t1 Lin
    | R(e'), LSum(t1, t2), Lin -> check e' t2 Lin
    | L(e'), ISum(t1, t2), Int -> check e' t1 Int
    | R(e'), ISum(t1, t2), Int -> check e' t2 Int
    | Case (e', x1, e1, x2, e2), _ , Lin -> let* t1, t2 = infer e' Lin >>> plsLSum in
                                            let* ctx = get in
                                            let* ctx1 = withvar (fresh x1 t1) (check e1 t Lin) >> get in
                                            let* ctx2 = set ctx >> withvar (fresh x2 t2) (check e2 t Lin) >> get in
                                            if same ctx1 ctx2 then return () (*make same also monadic and make it error with a diff*)
                                                                else error (fstring "Different resulting contexts in linear case statement %s:\n%s\n%s" (printexpr e) (printctx ctx1) (printctx ctx2))                                       
    | Case (e', x1, e1, x2, e2), _ , Int -> let* t1, t2 = infer e' Int >>> plsISum in
                                            let* ctx = get in
                                            withvar (int x1 t1) (check e1 t Int) >> set ctx >> (*is this necessary?*)
                                            withvar (int x2 t2) (check e2 t Int)
    | LetF (x, e1, e2), _, Lin -> let* t' = infer e1 Lin >>> plsF in
                                  withvar (int x t') (check e2 t Lin)
    | EG(e'), G(t'), Int -> let* ctx = get in
                            lim ctx >>> set >> check e' t' Lin >> get >>> empty >> set ctx
    | EEvt(e'), Evt(t'), Lin -> check e' t' Lin
    | LetEvt(x, e1, e2), Evt(t'), Lin -> let* t1 = infer e1 Lin >>> plsEvt in
                                         let* ctx = get in
                                         lim ctx >>> set >> withvar (fresh x t1) (check e2 t Lin) >> set ctx
    | Let(x, e1, e2), _, ent -> let* t' = infer e1 ent in 
                                let s = (match ent with
                                        | Lin -> fresh x t'
                                        | Int -> int x t') in
                                withvar s (check e2 t ent)
    | Select(x1, x2, e1, e2, e1', e2'), Evt(t'), Lin -> let* t1 = infer e1 Lin >>> plsEvt in
                                                        let* t2 = infer e2 Lin >>> plsEvt in
                                                        let* ctx = get in
                                                        let* ctx1 = withvars ([fresh x1 t1; fresh x2 (Evt(t2))]) (check e1' t Lin) >> get in
                                                        let* ctx2 = set ctx >> withvars ([fresh x1 (Evt(t1)); fresh x2 t2]) (check e2' t Lin) >> get in
                                                        if same ctx1 ctx2 then return ()
                                                                          else error (fstring "Different resulting contexts in select statement %s" (printexpr e))
                                                        
    | _, _, _ -> let* t' = infer e ent ~attempt:t in
                 if t=t' then return ()
                         else error (fstring "Can't type check %s with %s in a %s context" (printexpr e) (printtype t) (printent ent))
and infer ?attempt (e: expr) (ent: ent) : typ t = print_endline (fstring "Infering %s in %s" (printexpr e) (printent ent));
    match e, ent with 
    | Var x, _ -> let* s = lookup_update x in (
                      match s.used with
                      | Fresh when ent=Lin -> return s.typ
                      | Inf when ent=Int -> return s.typ
                      | Used when ent=Lin -> error (fstring "Multiple usages of variable %s in linear context" x)
                      | Inf -> error (fstring "Improper usage of intuitionistic variable %s in linear context" x)
                      | _ -> error (fstring "Improper usage of linear variable %s in intuitionistic context" x)
                  )
    | Proj1(e'), Int -> let* t1, t2 = infer e' Int >>> plsProd in return t1
    | Proj2(e'), Int -> let* t1, t2 = infer e' Int >>> plsProd in return t2
    | Annot (e', t), _ -> check e' t ent >> return t
    | App (e1, e2), Int -> let* t1, t2 = infer e1 ent >>> plsArrow in
                           check e2 t1 ent >> return t2
    | App (e1, e2), Lin -> let* t1, t2 = infer e1 ent >>> plsLoli in
                           check e2 t1 ent >> return t2 
    | Run(e'), Lin -> infer e' Int >>> plsG >>> return
    | EF(e'), Lin -> let* ctx = get in
                     let* t' = lim ctx >>> set >> infer e' Int in
                     set ctx >> return (F t')
    | _ -> match attempt with
           | None   -> error (fstring "Can't infer type of %s" (printexpr e))
           | Some t -> error (fstring "Can't check %s against %s" (printexpr e) (printtype t))


let test1 = 
    let e = Lambda ("x", Unpair ("a", "b", Var "x", Pair (Var "b", Var "a"))) in
    let t = Loli (Tensor ((TVar "A"), (TVar "B")), Tensor ((TVar "B"), (TVar "A"))) in
    let r = infer (Annot(e, t)) Lin [] in
    match r with
    | Value (r, c) -> print_endline (fstring "Test1: %s has type %s, context left %s" (printexpr e) (printtype r) (printctx c))
    | Error e -> print_endline e

let test2 = 
    let t = Arrow (ISum (G(TVar "A"), G(TVar "B")),
                   G (LSum (TVar "A", TVar "B"))) in
    let e = Lambda("s", Case(Var "s", 
                             "x", EG(L(Run(Var "x"))),
                             "y", EG(R(Run(Var "y"))))) in
    let r = infer (Annot(e, t)) Int [] in
    match r with
    | Value (r, c) -> print_endline (fstring "Test2: %s has type %s, context left %s" (printexpr e) (printtype r) (printctx c))
    | Error e -> print_endline e

let test3 = 
    let t = Loli (LSum (TVar "A", TVar "B"),
                  LSum (TVar "B", TVar "A")) in
    let e = Lambda ("s", Case(Var "s", "x", R(Var "x"), "y", L(Var "y"))) in
    let r = infer (Annot(e, t)) Lin [] in
    match r with
    | Value (r, c) -> print_endline (fstring "Test3: %s has type %s, context left %s" (printexpr e) (printtype r) (printctx c))
    | Error e -> print_endline e

let test4 = 
    let t = Loli (Tensor (TVar "A", Loli(TVar "A", TVar "B")),
                  TVar "B") in
    let e = Lambda ("p", Unpair("x", "y", Var "p",
                         App (Var "y", Var "x"))) in
    let r = infer (Annot(e, t)) Lin [] in
    match r with
    | Value (r, c) -> print_endline (fstring "Test4: %s has type %s, context left %s" (printexpr e) (printtype r) (printctx c))
    | Error e -> print_endline e

let test5 = 
    let t = Arrow(G(F(G(TVar "A"))),
                  G(TVar "A")) in 
    let e = Lambda("g", EG( LetF("x", Run(Var "g"),
                            Run(Var "x")))) in
    let r = infer (Annot(e, t)) Int [] in
    match r with
    | Value (r, c) -> print_endline (fstring "Test5: %s has type %s, context left %s" (printexpr e) (printtype r) (printctx c))
    | Error e -> print_endline e

let test6 = 
    let t = Loli(F(G(F(TVar "X"))),
                 F(TVar "X")) in
    let e = Lambda("f", LetF("x", Var "f", Run(Var "x"))) in
    let r = infer (Annot(e, t)) Lin [] in
    match r with
    | Value (r, c) -> print_endline (fstring "Test6: %s has type %s, context left %s" (printexpr e) (printtype r) (printctx c))
    | Error e -> print_endline e


let tests = [test1; test2; test3; test4; test5; test6]

let rec run t =
    match t with
    | [] -> ()
    | t::ts -> t; run ts;;

let test _ = run tests