exception Unimplemented
exception UnexpectedError
type var = string
val fstring : ('a, unit, string) format -> 'a
type typ =
    LUnit
  | Loli of typ * typ
  | Tensor of typ * typ
  | LSum of typ * typ
  | F of typ
  | IUnit
  | Arrow of typ * typ
  | Prod of typ * typ
  | ISum of typ * typ
  | G of typ
val printtype : typ -> string
type expr =
    EUnit
  | Var of var
  | Lambda of var * expr
  | App of expr * expr
  | Pair of expr * expr
  | Unpair of var * var * expr * expr
  | Annot of expr * typ
  | L of expr
  | R of expr
  | Case of expr * var * expr * var * expr
  | Proj1 of expr
  | Proj2 of expr
  | EF of expr
  | EG of expr
  | Run of expr
  | LetF of var * expr * expr
val printexpr : expr -> var
type usage = Used | Fresh | Inf
type state = { var : var; used : usage; typ : typ; }
val mkstate : var -> usage -> typ -> state
val fresh : var -> typ -> state
val int : var -> typ -> state
type ctx = state list
val printusage : usage -> string
val printstate : state -> string
val printctx : state list -> string
type errorinfo = var
type 'a result = Value of 'a | Error of errorinfo
type 'a t = ctx -> ('a * ctx) result
val return : 'a -> 'a t
val error : errorinfo -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >> ) : unit t -> 'a t -> 'a t
val ( >>> ) : 'a t -> ('a -> 'b t) -> 'b t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val get : ctx t
val set : ctx -> unit t
val lookup : var -> state t
val lookup_update : var -> state t
val withvar : state -> 'a t -> 'a t
val withvars : state list -> unit t -> unit t
val rm : 'a list -> 'a -> 'a list
val find : 'a list -> 'a -> bool
val same : ctx -> ctx -> bool
val empty : ctx -> unit t
val plsProd : typ -> (typ * typ) t
val plsTensor : typ -> (typ * typ) t
val plsArrow : typ -> (typ * typ) t
val plsLoli : typ -> (typ * typ) t
val plsSum : typ -> (typ * typ) t
val plsLSum : typ -> (typ * typ) t
val plsISum : typ -> (typ * typ) t
val plsF : typ -> typ t
val plsG : typ -> typ t
val lim : ctx -> ctx t
type ent = Lin | Int
val printent : ent -> string
val check : expr -> typ -> ent -> unit t
val infer : expr -> ent -> typ t
