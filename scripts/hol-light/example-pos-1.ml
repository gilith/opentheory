let lemma x = x ();;
let theorem x = x ();;

new_type ("P",0);;

let t = `t : P -> P -> P` in
new_constant ("addP", type_of t);;

let t = `t : P -> P -> P` in
new_constant ("subP", type_of t);;

let t = `t : P -> P -> P` in
new_constant ("multP", type_of t);;

let t = `t : P -> P -> bool` in
new_constant ("leP", type_of t);;

let ADDP_COMM = new_axiom (`!x y. addP x y = addP y x`);;

let ADDP_ASSOC = new_axiom (`!x y z. addP (addP x y) z = addP x (addP y z)`);;

let SUBP_ELIM = new_axiom (`!x y. ~leP y x ==> addP x (subP y x) = y`);;

let MULTP_COMM = new_axiom (`!x y. multP x y = multP y x`);;

let MULTP_ASSOC = new_axiom
    (`!x y z. multP (multP x y) z = multP x (multP y z)`);;

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
       (if leP x y then if leP y x then Zero else Pos (subP y x)
        else Neg (subP x y)) /\
     add Zero (Neg y) = Neg y /\
     add Zero Zero = Zero /\
     add Zero (Pos y) = Pos y /\
     add (Pos x) (Neg y) =
       (if leP x y then if leP y x then Zero else Neg (subP y x)
        else Pos (subP x y)) /\
     add (Pos x) Zero = Pos x /\
     add (Pos x) (Pos y) = Pos (addP x y)`);;

let sub_def = theorem (fun () -> define
    `sub x y = add x (neg y)`);;

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

let INJECT_CASES = theorem (fun () -> prove
    (`!p.
        (!x. p (inject x)) /\ p Zero /\ (!x. p (neg (inject x))) ==>
        !x. p x`,
     REWRITE_TAC [inject_def; neg_def] THEN
     MESON_TAC [N_INDUCT]));;

let ADD_INJECT = theorem (fun () -> prove
    (`!x y. inject (addP x y) = add (inject x) (inject y)`,
     REWRITE_TAC [inject_def; add_def]));;

let SUB_INJECT = theorem (fun () -> prove
    (`!x y. ~leP x y ==> inject (subP x y) = sub (inject x) (inject y)`,
     REWRITE_TAC [inject_def; neg_def; add_def; sub_def] THEN
     REPEAT STRIP_TAC THEN
     ASM_REWRITE_TAC []));;

let MULT_INJECT = theorem (fun () -> prove
    (`!x y. inject (multP x y) = mult (inject x) (inject y)`,
     REWRITE_TAC [inject_def; mult_def]));;

let LE_INJECT = theorem (fun () -> prove
    (`!x y. leP x y <=> le (inject x) (inject y)`,
     REWRITE_TAC [inject_def; le_def]));;

let ADD_COMM = theorem (fun () -> prove
    (`!x y. add x y = add y x`,
     MATCH_MP_TAC N_INDUCT THEN
     REPEAT CONJ_TAC THENL
     [GEN_TAC THEN
      MATCH_MP_TAC N_INDUCT THEN
      REWRITE_TAC [add_def] THEN
      CONJ_TAC THENL
      [MESON_TAC [ADDP_COMM],
       X_GEN_TAC `b:P` THEN
       
));;

(*
let ADD_ASSOC = new_axiom (`!x y z. add (add x y) z = add x (add y z)`);;

let SUB_ELIM = new_axiom (`!x y. add x (sub y x) = y`);;

let MULT_COMM = new_axiom (`!x y. mult x y = mult y x`);;

let MULT_ASSOC = new_axiom
    (`!x y z. mult (mult x y) z = mult x (mult y z)`);;
*)
