open Checking

let test1 = 
    let e = Lambda ("x", Unpair ("a", "b", Var "x", Pair (Var "a", Var "b"))) in
    let t = Loli (Tensor (LUnit, LUnit), Tensor (LUnit, LUnit)) in
infer Annot(e, t) []