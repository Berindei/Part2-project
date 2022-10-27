exception Unimplemented
exception UnexpectedError
type var = string
val fstring : ('a, unit, string) format -> 'a
type typ = TUnit | Loli of typ * typ | Tensor of typ * typ | Sum of typ * typ
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
val printexpr : expr -> var
type usage = Used | Fresh
type state = { var : var; used : usage; typ : typ; }
val mkstate : var -> usage -> typ -> state
val fresh : var -> typ -> state
type ctx = state list
type errorinfo = var
type 'a result = Value of 'a | Error of errorinfo
type 'a t = ctx -> ('a * ctx) result
val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val empty : ctx -> bool
val lookup : var -> state t
val lookup_update : var -> state t
val rm : 'a list -> 'a -> 'a list
val find : 'a list -> 'a -> bool
val same : ctx -> ctx -> bool
val check : expr -> typ -> unit t
val infer : expr -> typ t
val typecheck : expr -> typ t
