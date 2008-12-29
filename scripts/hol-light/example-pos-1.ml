new_type ("P",0);;

let t = `t : P -> P -> bool` in
new_constant ("leP", type_of t);;

let t = `t : P -> P -> P` in
new_constant ("addP", type_of t);;

let t = `t : P -> P -> P` in
new_constant ("multP", type_of t);;

let t = `t : P -> P -> P` in
new_constant ("subP", type_of t);;

let N_INDUCT,N_RECURSION = define_type
  "N = Neg P | Zero | Pos P";;

let inject_def = lemma (fun () -> define
    `inject x = Pos x`);;

let neg_def = lemma (fun () -> define
    `neg (Neg x) = Pos x /\
     neg Zero = Zero /\
     neg (Pos x) = Neg x`);;

let add_def = lemma (fun () -> define
    `add (Neg x) (Neg y) = Neg (addP x y) /\
     add (Neg x) Zero = Neg x /\
     add (Neg x) (Pos y) =
       if leP x y then if leP y x then Zero else Pos (subP y x)
       else Neg (subP x y) /\
     add Zero (Neg y) = Neg y /\
     add Zero Zero = Zero /\
     add Zero (Pos y) = Pos y /\
     add (Pos x) (Neg y) =
       if leP x y then if leP y x then Zero else Neg (subP y x)
       else Pos (subP x y) /\
     add (Pos x) Zero = Pos x /\
     add (Pos x) (Pos y) = Pos (addP x y)`);;

let mult_def = lemma (fun () -> define
    `mult (Neg x) (Neg y) = Pos (multP x y) /\
     mult (Neg x) Zero = Zero /\
     mult (Neg x) (Pos y) = Neg (multP x y) /\
     mult Zero (Neg y) = Zero /\
     mult Zero Zero = Zero /\
     mult Zero (Pos y) = Zero /\
     mult (Pos x) (Neg y) = Neg (multP x y) /\
     mult (Pos x) Zero = Zero /\
     mult (Pos x) (Pos y) = Pos (multP x y)`);;

let le_def = lemma (fun () -> define
    `le (Neg x) (Neg y) = leP y x /\
     le (Neg x) Zero = T /\
     le (Neg x) (Pos y) = T /\
     le Zero (Neg y) = F /\
     le Zero Zero = T /\
     le Zero (Pos y) = T /\
     le (Pos x) (Neg y) = F /\
     le (Pos x) Zero = F /\
     le (Pos x) (Pos y) = leP x y`);;

let LE_INJECT = theorem (fun () -> prove
    (`!x y. leP x y <=> le (inject x) (inject y)`,
