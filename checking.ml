exception Unimplemented

exception UnexpectedError

type var = string
type time = int

let fstring = Printf.sprintf

let filtermap l f p = List.map (fun x -> if p x then f x else x) l

type indx = 
    | IVar of var
    | Time of time

type indxtype = 
    | (*Ho-kago*) TTime

let rec printindx i =
    match i with 
    | IVar x -> x
    | Time t -> string_of_int t

let rec printindxtype t = 
    match t with
    | TTime -> "Time" 

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
    | IndxT  of indxtype
    | At     of typ * indx
    | Univ   of var * indxtype * typ
    | Exist  of var * indxtype * typ

let rec printtype t = 
    match t with
    | TVar x            -> x
    | LUnit             -> "I"
    | Loli (t1, t2)     -> fstring "(%s⊸%s)" (printtype t1) (printtype t2)
    | Tensor (t1, t2)   -> fstring "(%s⊗%s)" (printtype t1) (printtype t2)
    | LSum (t1, t2)     -> fstring "(%s⊕%s)" (printtype t1) (printtype t2)
    | F t'              -> fstring "F(%s)" (printtype t')
    | Evt t'            -> fstring "♢(%s)" (printtype t')
    | IUnit             -> "1"
    | Arrow (t1, t2)    -> fstring "(%s→%s)" (printtype t1) (printtype t2)
    | Prod (t1, t2)     -> fstring "(%s*%s)" (printtype t1) (printtype t2)
    | ISum (t1, t2)     -> fstring "(%s+%s)" (printtype t1) (printtype t2)
    | G t'              -> fstring "G(%s)" (printtype t')
    | IndxT i           -> printindxtype i
    | At (t1, t')       -> fstring "%s@%s" (printtype t1) (printindx t')
    | Univ (x, t1, t2)  -> fstring "∀%s:%s. %s" x (printindxtype t1) (printtype t2)
    | Exist (x, t1, t2) -> fstring "∃%s:%s. %s" x (printindxtype t1) (printtype t2)
    

type expr = 
    | EUnit
    | Var        of var
    | Lambda     of var * expr
    | App        of expr * expr
    | Pair       of expr * expr
    | Unpair     of var * var * expr * expr
    | Annot      of expr * typ
    | L          of expr
    | R          of expr
    | Case       of expr * var * expr * var * expr
    | Proj1      of expr
    | Proj2      of expr
    | EF         of expr
    | EG         of expr
    | Run        of expr
    | LetF       of var * expr * expr
    | EEvt       of expr
    | LetEvt     of var * expr * expr
    | Let        of var * expr * expr
    | Select     of var * var * expr * expr * expr * expr
    | EAt        of expr
    | LetAt      of var * expr * expr
    | LambdaIndx of var * expr
    | AppIndx    of expr * indx
    | Pack       of indx * expr
    | LetPack    of var * var * expr * expr
    | Indx       of indx

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
    | EAt e'                          -> fstring "@(%s)" (printexpr e')
    | LetAt (x, e1, e2)               -> fstring "let @%s = %s in %s" x (printexpr e1) (printexpr e2)
    | LambdaIndx (x, e')              -> fstring "Λ%s. %S" x (printexpr e')
    | AppIndx (e1, e2)                -> fstring "(%s [%s])" (printexpr e1) (printindx e2)
    | Pack (e1, e2)                   -> fstring "pack(%s, %s)" (printindx e1) (printexpr e2)
    | LetPack (i, x, e1, e2)          -> fstring "let pack(%s, %s) = %s in %s" i x (printexpr e1) (printexpr e2)
    | Indx (indx)                     -> printindx indx

type usage = Used | Fresh | Inf
type flag = Use | Ignore

type state = {var: var; used: usage; typ: typ; ignore: flag; delay: flag list; at: indx}

let mkstate v u t i d a = {var=v; used=u; typ=t; ignore=i; delay=d; at=a}

let used v t :state= mkstate v Used t Use [Use] (Time 0)
let fresh v t = mkstate v Fresh t Use [Use] (Time 0)
let int v t = mkstate v Inf t Use [Use] (Time 0) (*includes indeces*)

let delayed v t d = {(fresh v t) with at = d}

let ignore s = s.ignore = Ignore || List.exists (fun x -> x=Ignore) s.delay
let use s = s.ignore = Use && List.for_all (fun x -> x=Use) s.delay

let actualAt s = if List.length s.delay <= 1 then s.at else Time 0

type ctx = state list

let printusage u = match u with Fresh->"1"| Used->"0" | Inf->"∞"

let printstate s =
    let printinner =
        match s.used with
        | _ when s.at = Time 0 -> fstring "%s^%s: %s" s.var (printusage s.used) (printtype s.typ)
        | Inf -> fstring "%s^%s: %s" s.var (printusage s.used) (printtype s.typ)
        | Used | Fresh -> fstring "%s^%s: %s[%s]" s.var (printusage s.used) (printtype s.typ) (printindx (actualAt s))
    in let i = printinner in
    match s.ignore, List.exists (fun x -> x=Ignore) s.delay with
    | Use, false -> i
    | Ignore, false -> fstring "#%s#" i
    | Use, true -> fstring "@%s@" i
    | Ignore, true -> fstring "#@%s@#" i

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
    | y :: ys when x = y.var && ignore y -> Error (fstring "Variable %s not available in this context" x)
    | y :: ys when x = y.var -> Value (y, y :: ys)
    | y :: ys                -> (lookup x >>= (fun s -> fun ctx' -> Value(s, y :: ctx'))) ys

let rec lookup_update: var -> state t = fun (x: var) -> fun (ctx: ctx) ->
    match ctx with
    | []                     -> Error (fstring "Variable %s not in context" x)
    | y :: ys when x = y.var && ignore y -> Error (fstring "Variable %s not available in this context" x)
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
    match List.find_opt (fun s -> s.used=Fresh && use s) ctx with
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

let plsAt t =
    match t with
    | At(t, tau) -> return (t, tau)
    | _ -> error (unexpectedform "α@τ" t)

let plsUniv t =
    match t with
    | Univ(x, it, t) -> return (x, it, t)
    | _ -> error (unexpectedform "∀i:σ.α" t)

let plsExist t =
    match t with
    | Exist(x, it, t) -> return (x, it, t)
    | _ -> error (unexpectedform "∃i:σ.α" t)

let lim: ctx -> ctx t = fun ctx ->
    return (filtermap ctx (fun s -> {s with ignore=Ignore}) (fun s -> s.used!=Inf)) (*(List.map (fun s -> if s.used!=Inf then {s with incl=Ignore} else s) ctx)*)

let nolin: 'a t -> 'a t = fun m ->
                          let* ctx = get in
                          let* r = lim ctx >>> set >> m in
                          get >>> empty >> set ctx >> return r

(*figure out delay somehow, would need to change at somehow*)
(*Might work now idk*)

let delay: indx -> 'a t -> 'a t = fun t -> fun m ->
                                  let* ctx = get in
                                  let ctx' = List.map (fun s -> {s with delay = (if actualAt s = t then Use else Ignore)::s.delay}) ctx in
                                  let* r = set ctx' >> m in
                                  let* ctx'' = get in
                                  set (List.map (fun s -> {s with delay = (List.tl s.delay)}) ctx'') >> return r

(* let timed: ctx -> time -> ctx t = fun ctx -> fun t ->
    return (filtermap ctx (fun s -> {s with incl=Ignore}) (fun s -> (s.used=Fresh || s.used=Used) && s.delay!=t)) *)

type ent = Lin | Int | Ind

let printent e = match e with Lin->"linear" | Int->"intuitionistic" | Ind->"index"

let rec sub: var -> indx -> typ -> typ = fun x -> fun s -> fun t ->
    let subber = sub x s in
    match t with
    | TVar x -> TVar x
    | LUnit -> LUnit
    | Loli (x, y) -> Loli (subber x, subber y)
    | Tensor (x, y) -> Tensor (subber x, subber y)
    | LSum (x, y) -> LSum (subber x, subber y)
    | F x -> F (subber x)
    | Evt x -> Evt (subber x)
    | IUnit -> IUnit
    | Arrow (x, y) -> Arrow (subber x, subber y)
    | Prod (x, y) -> Prod (subber x, subber y)
    | ISum (x, y) -> ISum (subber x, subber y)
    | G x -> G (subber x)
    | IndxT x -> IndxT x
    | At (t', IVar y) when x=y -> At (t', s)
    | At (x, y) -> At (subber x, y)
    | Univ (x, y, z) -> Univ (x, y, subber z)
    | Exist (x, y, z) -> Exist (x, y, subber z)

let rec check (e: expr) (t: typ) (ent: ent) : unit t = print_endline (fstring "Checking %s against %s in %s" (printexpr e) (printtype t) (printent ent));
    match e, t, ent with
    | Indx(Time _), IndxT TTime, Ind -> return ()
    | Indx(IVar x), IndxT _, Ind -> let* s = lookup x in if s.typ = t then return () else raise UnexpectedError
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
    | EG(e'), G(t'), Int -> nolin (check e' t' Lin)
    | EEvt(e'), Evt(t'), Lin -> check e' t' Lin
    | LetEvt(x, e1, e2), Evt(_), Lin -> let* t1 = infer e1 Lin >>> plsEvt in
                                             nolin (withvar (fresh x t1) (check e2 t Lin))
    | Let(x, e1, e2), _, ent -> let* t' = infer e1 ent in 
                                let s = (match ent with
                                        | Lin -> fresh x t'
                                        | Int -> int x t'
                                        | Ind -> failwith "TODO: error not allowed") in
                                withvar s (check e2 t ent)
    | Select(x1, x2, e1, e2, e1', e2'), Evt(t'), Lin -> let* t1 = infer e1 Lin >>> plsEvt in
                                                        let* t2 = infer e2 Lin >>> plsEvt in
                                                        nolin ( let* ctx = get in
                                                                let* ctx1 = withvars ([fresh x1 t1; fresh x2 (Evt(t2))]) (check e1' t Lin) >> get in
                                                                let* ctx2 = set ctx >> withvars ([fresh x1 (Evt(t1)); fresh x2 t2]) (check e2' t Lin) >> get in
                                                                if same ctx1 ctx2 then return ()
                                                                                else error (fstring "Different resulting contexts in select statement %s" (printexpr e)))                                                        
    | EAt(e')(*might want to add time to expr but not necessary*), At(t', i), Lin -> check (Indx i) (IndxT TTime) Ind >> delay i (check e' t' Lin)
    | LambdaIndx(i, e'), Univ(ti, it, t'), Lin  when i=ti-> withvar (int i (IndxT it)) (check e' t' Lin) (*if diff, gen new name*)
    | LetAt(x, e1, e2), _, Lin -> let* t', tau = infer e1 Lin >>> plsAt in
                                  withvar (delayed x t' tau) (check e2 t Lin)
    | Pack(i, e'), Exist(ti, it, t'), Lin -> check (Indx i) (IndxT it) Ind >> check e' (sub ti i t') Lin
    | LetPack(x1, x2, e1, e2), _, Lin -> let* x, it, t' = infer e1 Lin >>> plsExist in
                                         if x != x1 then error (fstring "") else
                                         withvars [int x1 (IndxT it); fresh x2 t'] (check e2 t Lin)
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
    | Run(e'), Lin -> nolin (infer e' Int >>> plsG >>> return)
    | EF(e'), Lin -> nolin ( let* t' = infer e' Int in return (F t'))
    | AppIndx(e', s), Lin -> let* i, it, t' = infer e' Lin >>> plsUniv in
                             check (Indx s) (IndxT it) Ind >> return (sub i s t')
    | _ -> match attempt with
           | None   -> error (fstring "Can't infer type of %s" (printexpr e))
           | Some t -> error (fstring "Can't check %s against %s" (printexpr e) (printtype t))


let rec compile e =
    match e with
    | EUnit -> "()"
    | Var x -> x
    | Lambda(x, m) -> let m' = compile m in
                      fstring "function(%s){return (
%s
)}" x m'
    | App(m, n) -> let m' = (compile m) in
                   let n' = compile n in
                   fstring "(%s)(%s)" m' n'
    | Pair(e1, e2) -> let e1' = compile e1 in
                      let e2' = compile e2 in
                      fstring "[%s, %s]" e1' e2'
    | Unpair(x1, x2, e1, e2) -> let e1' = compile e1 in
                                let e2' = compile e2 in
                                fstring (
"function() {
    var pair = %s;
    return (function(%s, %s){
        return(%s)
    })(pair[0], pair[1])
}()") e1' x1 x2 e2'
    | Annot(e1, t) -> compile e1
    | L(e1) -> let e1' = compile e1 in
               fstring "{value: %s, tag:\"L\"}" e1'
    | R(e1) -> let e1' = compile e1 in
               fstring "{value: %s, tag:\"R\"}" e1'
    | Case(e1, x1, e2, x2, e3) -> let e1' = compile e1 in let e2' = compile e2 in let e3' = compile e3 in
                                  fstring
"(function(){
    var sum = %s;
    var f1 = function (%s) {return (%s)};
    var f2 = function (%s) {return (%s)};
    if(sum.tag==\"L\") return f1(sum.value);
    else return f2(sum.value);
})()" e1' x1 e2' x2 e3'
    | Proj1(e1) -> (compile e1) ^ "[0]"
    | Proj2(e1) -> (compile e1) ^ "[1]"
    | EF(e1) -> compile e1
    | EG(e1) -> let e1' = compile e1 in 
                fstring "function(){return (%s)}" e1'
    | LetF(x, e1, e2) -> let e1' = compile e1 in
                         let e2' = compile e2 in
                         fstring "(function(%s){return(%s)})(
%s)" x e2' e1'
    | Run(e1) -> let e1'= compile e1 in
                 fstring "(%s)()" e1'
    | EEvt(e1) -> let e1'= compile e1 in
                 fstring "(function(){
    var chan = new Channel();
    chan.put(%s);
    return chan.get
})()" e1'
    | LetEvt(x, e1, e2) -> let e1' = compile e1 in 
                           let e2' = compile e2 in
                           fstring "(function(){
    var chan = new Channel();
    var g = %s;
    var f = %s => (%s);
    g( x => f(x)(y => chan.put(y)) );
    return chan.get;
})()" e2' x e1'
    | Select(x, y, e1, e2, e3, e4) -> let e1' = compile e1 in
                                      let e2' = compile e2 in
                                      let e3' = compile e3 in 
                                      let e4' = compile e4 in
                                      fstring "function (){
    var chan = new Channel();
    var g1 = %s;
    var g2 = %s;
    var f1 = %s => (%s);
    var f2 = %s => (%s);
    var first = true;
    g1( x=> f1(x)( y => if (first){ chan.put(y); first = false;} ) );
    g2( x=> f2(x)( y => if (first){ chan.put(y); first = false;} ) );
})()" e1' e2' x e3' y e4'
    | EAt(e1) -> compile (EEvt e1)
    | LetAt(x, e1, e2) -> compile (LetEvt(x, e1, e2))
    | LambdaIndx(x, e1) -> let e1' = compile e1 in
                           fstring "function (){return (%s)}" e1'
    | AppIndx(e1, e2) -> let e1' = compile e1 in
                         fstring "(%s)()" e1'
    | Pack(e1, e2) -> let e2' = compile e2 in
                      fstring "(function (){return (%s)})()" e2'
    | LetPack(x, i, e1, e2) -> let e1' = compile e1 in
                               let e2' = compile e2 in
                               fstring "(function(%s){return (%s)})(%s)" x e2' e1'
    | Let(x, e1, e2) -> let e1' = compile e1 in let e2' = compile e2 in
                        fstring "(function(%s){return (%s)})(%s)" x e2' e1'
    | Indx(x) -> failwith "This shouldn't happen"

let generate e = let out = open_out "generated.js" in
                 let js = compile e in print_endline js;
                 output_string out (fstring "function main(){return (\n\n%s\n\n)};" js);
                 close_out out

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