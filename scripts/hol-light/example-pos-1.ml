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

let LEP_REFL = new_axiom (`!x. leP x x`);;

let LEP_TRANS = new_axiom (`!x y z. leP x y /\ leP y z ==> leP x z`);;

let LEP_ANTISYM = new_axiom (`!x y. leP x y /\ leP y x ==> x = y`);;

let LEP_TOTAL = new_axiom (`!x y. leP x y \/ leP y x`);;

let N_INDUCT,N_RECURSION = define_type
  "N = Neg P | Zero | Pos P";;

let N_DISTINCT = distinctness "N";;

let N_INJECTIVE = injectivity "N";;

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

let INJECT_NOT_ZERO = theorem (fun () -> prove
    (`!x. ~(inject x = Zero)`,
     REWRITE_TAC [inject_def] THEN
     MESON_TAC [N_DISTINCT]));;

let INJECT_NOT_NEG = theorem (fun () -> prove
    (`!x y. ~(inject x = neg (inject y))`,
     REWRITE_TAC [inject_def; neg_def] THEN
     MESON_TAC [N_DISTINCT]));;

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

let NEG_ZERO = theorem (fun () -> prove
    (`neg Zero = Zero`,
     REWRITE_TAC [neg_def]));;

let NEG_IS_ZERO = theorem (fun () -> prove
    (`!x. neg x = Zero <=> x = Zero`,
     MATCH_MP_TAC N_INDUCT THEN
     REWRITE_TAC [neg_def] THEN
     MESON_TAC [N_DISTINCT]));;

let NEG_NEG = theorem (fun () -> prove
    (`!x. neg (neg x) = x`,
     MATCH_MP_TAC N_INDUCT THEN
     REWRITE_TAC [neg_def]));;

let LE_REFL = theorem (fun () -> prove
    (`!x. le x x`,
     MATCH_MP_TAC N_INDUCT THEN
     REWRITE_TAC [le_def; LEP_REFL]));;

let LE_TRANS = theorem (fun () -> prove
    (`!x y z. le x y /\ le y z ==> le x z`,
     (let tac1 =
          X_GEN_TAC `x:P` THEN
          MATCH_MP_TAC N_INDUCT THEN
          REPEAT CONJ_TAC THENL
          [X_GEN_TAC `y:P` THEN
           MATCH_MP_TAC N_INDUCT THEN
           REPEAT CONJ_TAC THENL
           [X_GEN_TAC `z:P` THEN
            REWRITE_TAC [le_def] THEN
            MESON_TAC [LEP_TRANS];
            REWRITE_TAC [le_def];
            X_GEN_TAC `z:P` THEN
            REWRITE_TAC [le_def]];
           MATCH_MP_TAC N_INDUCT THEN
           REPEAT CONJ_TAC THENL
           [X_GEN_TAC `z:P` THEN
            REWRITE_TAC [le_def];
            REWRITE_TAC [le_def];
            X_GEN_TAC `z:P` THEN
            REWRITE_TAC [le_def]];
           X_GEN_TAC `y:P` THEN
           MATCH_MP_TAC N_INDUCT THEN
           REPEAT CONJ_TAC THENL
           [X_GEN_TAC `z:P` THEN
            REWRITE_TAC [le_def];
            REWRITE_TAC [le_def];
            X_GEN_TAC `z:P` THEN
            REWRITE_TAC [le_def]]] in
      let tac2 =
          MATCH_MP_TAC N_INDUCT THEN
          REPEAT CONJ_TAC THENL
          [X_GEN_TAC `y:P` THEN
           MATCH_MP_TAC N_INDUCT THEN
           REPEAT CONJ_TAC THENL
           [X_GEN_TAC `z:P` THEN
            REWRITE_TAC [le_def];
            REWRITE_TAC [le_def];
            X_GEN_TAC `z:P` THEN
            REWRITE_TAC [le_def]];
           MESON_TAC [];
           X_GEN_TAC `y:P` THEN
           MATCH_MP_TAC N_INDUCT THEN
           REPEAT CONJ_TAC THENL
           [X_GEN_TAC `z:P` THEN
            REWRITE_TAC [le_def];
            REWRITE_TAC [le_def];
            X_GEN_TAC `z:P` THEN
            REWRITE_TAC [le_def]]] in
      let tac3 =
          X_GEN_TAC `x:P` THEN
          MATCH_MP_TAC N_INDUCT THEN
          REPEAT CONJ_TAC THENL
          [X_GEN_TAC `y:P` THEN
           MATCH_MP_TAC N_INDUCT THEN
           REPEAT CONJ_TAC THENL
           [X_GEN_TAC `z:P` THEN
            REWRITE_TAC [le_def];
            REWRITE_TAC [le_def];
            X_GEN_TAC `z:P` THEN
            REWRITE_TAC [le_def]];
           MATCH_MP_TAC N_INDUCT THEN
           REPEAT CONJ_TAC THENL
           [X_GEN_TAC `z:P` THEN
            REWRITE_TAC [le_def];
            REWRITE_TAC [le_def];
            X_GEN_TAC `z:P` THEN
            REWRITE_TAC [le_def]];
           X_GEN_TAC `y:P` THEN
           MATCH_MP_TAC N_INDUCT THEN
           REPEAT CONJ_TAC THENL
           [X_GEN_TAC `z:P` THEN
            REWRITE_TAC [le_def];
            REWRITE_TAC [le_def];
            X_GEN_TAC `z:P` THEN
            REWRITE_TAC [le_def] THEN
            MESON_TAC [LEP_TRANS]]] in
      MATCH_MP_TAC N_INDUCT THEN
      REPEAT CONJ_TAC THENL
      [tac1;
       tac2;
       tac3])));;

let LE_ANTISYM = theorem (fun () -> prove
    (`!x y. le x y /\ le y x ==> x = y`,
     MATCH_MP_TAC N_INDUCT THEN
     REPEAT CONJ_TAC THENL
     [X_GEN_TAC `x:P` THEN
      MATCH_MP_TAC N_INDUCT THEN
      REPEAT CONJ_TAC THENL
      [X_GEN_TAC `y:P` THEN
       REWRITE_TAC [le_def; N_INJECTIVE] THEN
       MESON_TAC [LEP_ANTISYM];
       REWRITE_TAC [le_def];
       X_GEN_TAC `y:P` THEN
       REWRITE_TAC [le_def]];
      MATCH_MP_TAC N_INDUCT THEN
      REPEAT CONJ_TAC THENL
      [X_GEN_TAC `y:P` THEN
       REWRITE_TAC [le_def];
       REWRITE_TAC [le_def];
       X_GEN_TAC `y:P` THEN
       REWRITE_TAC [le_def]];
      X_GEN_TAC `x:P` THEN
      MATCH_MP_TAC N_INDUCT THEN
      REPEAT CONJ_TAC THENL
      [X_GEN_TAC `y:P` THEN
       REWRITE_TAC [le_def];
       REWRITE_TAC [le_def];
       X_GEN_TAC `y:P` THEN
       REWRITE_TAC [le_def; N_INJECTIVE] THEN
       MESON_TAC [LEP_ANTISYM]]]));;

let LE_TOTAL = theorem (fun () -> prove
    (`!x y. le x y \/ le y x`,
     MATCH_MP_TAC N_INDUCT THEN
     REPEAT CONJ_TAC THENL
     [X_GEN_TAC `x:P` THEN
      MATCH_MP_TAC N_INDUCT THEN
      REPEAT CONJ_TAC THENL
      [X_GEN_TAC `y:P` THEN
       REWRITE_TAC [le_def] THEN
       MESON_TAC [LEP_TOTAL];
       REWRITE_TAC [le_def];
       X_GEN_TAC `y:P` THEN
       REWRITE_TAC [le_def]];
      MATCH_MP_TAC N_INDUCT THEN
      REPEAT CONJ_TAC THENL
      [X_GEN_TAC `y:P` THEN
       REWRITE_TAC [le_def];
       REWRITE_TAC [le_def];
       X_GEN_TAC `y:P` THEN
       REWRITE_TAC [le_def]];
      X_GEN_TAC `x:P` THEN
      MATCH_MP_TAC N_INDUCT THEN
      REPEAT CONJ_TAC THENL
      [X_GEN_TAC `y:P` THEN
       REWRITE_TAC [le_def];
       REWRITE_TAC [le_def];
       X_GEN_TAC `y:P` THEN
       REWRITE_TAC [le_def] THEN
       MESON_TAC [LEP_TOTAL]]]));;

let ADD_LZERO = theorem (fun () -> prove
    (`!x. add Zero x = x`,
     MATCH_MP_TAC N_INDUCT THEN
     REWRITE_TAC [add_def]));;

let ADD_RZERO = theorem (fun () -> prove
    (`!x. add x Zero = x`,
     MATCH_MP_TAC N_INDUCT THEN
     REWRITE_TAC [add_def]));;

let ADD_LNEG = theorem (fun () -> prove
    (`!x. add (neg x) x = Zero`,
     MATCH_MP_TAC N_INDUCT THEN
     REWRITE_TAC [neg_def; add_def; LEP_REFL]));;

let ADD_RNEG = theorem (fun () -> prove
    (`!x. add x (neg x) = Zero`,
     MATCH_MP_TAC N_INDUCT THEN
     REWRITE_TAC [neg_def; add_def; LEP_REFL]));;

let ADD_COMM = theorem (fun () -> prove
    (`!x y. add x y = add y x`,
     MATCH_MP_TAC N_INDUCT THEN
     REPEAT CONJ_TAC THENL
     [X_GEN_TAC `x:P` THEN
      MATCH_MP_TAC N_INDUCT THEN
      REPEAT CONJ_TAC THENL
      [X_GEN_TAC `y:P` THEN
       REWRITE_TAC [add_def; N_INJECTIVE] THEN
       MESON_TAC [ADDP_COMM];
       REWRITE_TAC [add_def];
       X_GEN_TAC `y:P` THEN
       REWRITE_TAC [add_def] THEN
       ASM_CASES_TAC `leP x y` THEN
       ASM_CASES_TAC `leP y x` THEN
       ASM_REWRITE_TAC [] THEN
       ASM_MESON_TAC [LEP_TOTAL]];
      MATCH_MP_TAC N_INDUCT THEN
      REPEAT CONJ_TAC THENL
      [X_GEN_TAC `y:P` THEN
       REWRITE_TAC [add_def];
       REWRITE_TAC [add_def];
       X_GEN_TAC `y:P` THEN
       REWRITE_TAC [add_def]];
      X_GEN_TAC `x:P` THEN
      MATCH_MP_TAC N_INDUCT THEN
      REPEAT CONJ_TAC THENL
      [X_GEN_TAC `y:P` THEN
       REWRITE_TAC [add_def] THEN
       ASM_CASES_TAC `leP x y` THEN
       ASM_CASES_TAC `leP y x` THEN
       ASM_REWRITE_TAC [] THEN
       ASM_MESON_TAC [LEP_TOTAL];
       REWRITE_TAC [add_def];
       X_GEN_TAC `y:P` THEN
       REWRITE_TAC [add_def; N_INJECTIVE] THEN
       MESON_TAC [ADDP_COMM]]]));;

(*
let ADD_ASSOC = theorem (fun () -> prove
    (`!x y z. add (add x y) z = add x (add y z)`,
     MATCH_MP_TAC N_INDUCT THEN
     REPEAT CONJ_TAC THENL
     [X_GEN_TAC `x:P` THEN
      MATCH_MP_TAC N_INDUCT THEN
      REPEAT CONJ_TAC THENL
      [X_GEN_TAC `y:P` THEN
       MATCH_MP_TAC N_INDUCT THEN
       REPEAT CONJ_TAC THENL
       [X_GEN_TAC `z:P` THEN
        REWRITE_TAC [add_def; N_INJECTIVE] THEN
        MESON_TAC [ADDP_ASSOC];
        REWRITE_TAC [add_def; N_INJECTIVE];
        X_GEN_TAC `z:P` THEN
        REWRITE_TAC [add_def]

let SUB_ELIM = theorem (fun () -> prove
    (`!x y. add x (sub y x) = y`,
     REWRITE_TAC [sub_def] THEN
     REPEAT GEN_TAC THEN
     CONV_TAC (LAND_CONV (RAND_CONV (REWR_CONV ADD_COMM))) THEN
     REWRITE_TAC [GSYM ADD_ASSOC; ADD_RNEG; ADD_LZERO]));;
*)

let MULT_LZERO = theorem (fun () -> prove
    (`!x. mult Zero x = Zero`,
     MATCH_MP_TAC N_INDUCT THEN
     REWRITE_TAC [mult_def]));;

let MULT_RZERO = theorem (fun () -> prove
    (`!x. mult x Zero = Zero`,
     MATCH_MP_TAC N_INDUCT THEN
     REWRITE_TAC [mult_def]));;

let MULT_COMM = theorem (fun () -> prove
    (`!x y. mult x y = mult y x`,
     MATCH_MP_TAC N_INDUCT THEN
     REPEAT CONJ_TAC THENL
     [X_GEN_TAC `x:P` THEN
      MATCH_MP_TAC N_INDUCT THEN
      REPEAT CONJ_TAC THENL
      [X_GEN_TAC `y:P` THEN
       REWRITE_TAC [mult_def; N_INJECTIVE] THEN
       MESON_TAC [MULTP_COMM];
       REWRITE_TAC [mult_def];
       X_GEN_TAC `y:P` THEN
       REWRITE_TAC [mult_def; N_INJECTIVE] THEN
       MESON_TAC [MULTP_COMM]];
      REWRITE_TAC [MULT_LZERO; MULT_RZERO];
      X_GEN_TAC `x:P` THEN
      MATCH_MP_TAC N_INDUCT THEN
      REPEAT CONJ_TAC THENL
      [X_GEN_TAC `y:P` THEN
       REWRITE_TAC [mult_def; N_INJECTIVE] THEN
       MESON_TAC [MULTP_COMM];
       REWRITE_TAC [mult_def];
       X_GEN_TAC `y:P` THEN
       REWRITE_TAC [mult_def; N_INJECTIVE] THEN
       MESON_TAC [MULTP_COMM]]]));;

let MULT_ASSOC = theorem (fun () -> prove
    (`!x y z. mult (mult x y) z = mult x (mult y z)`,
     MATCH_MP_TAC N_INDUCT THEN
     REPEAT CONJ_TAC THENL
     [X_GEN_TAC `x:P` THEN
      MATCH_MP_TAC N_INDUCT THEN
      REPEAT CONJ_TAC THENL
      [X_GEN_TAC `y:P` THEN
       MATCH_MP_TAC N_INDUCT THEN
       REPEAT CONJ_TAC THENL
       [X_GEN_TAC `z:P` THEN
        REWRITE_TAC [mult_def; N_INJECTIVE] THEN
        MESON_TAC [MULTP_ASSOC];
        REWRITE_TAC [MULT_LZERO; MULT_RZERO];
        X_GEN_TAC `z:P` THEN
        REWRITE_TAC [mult_def; N_INJECTIVE] THEN
        MESON_TAC [MULTP_ASSOC]];
       REWRITE_TAC [MULT_LZERO; MULT_RZERO];
       X_GEN_TAC `y:P` THEN
       MATCH_MP_TAC N_INDUCT THEN
       REPEAT CONJ_TAC THENL
       [X_GEN_TAC `z:P` THEN
        REWRITE_TAC [mult_def; N_INJECTIVE] THEN
        MESON_TAC [MULTP_ASSOC];
        REWRITE_TAC [MULT_LZERO; MULT_RZERO];
        X_GEN_TAC `z:P` THEN
        REWRITE_TAC [mult_def; N_INJECTIVE] THEN
        MESON_TAC [MULTP_ASSOC]]];
      REWRITE_TAC [MULT_LZERO; MULT_RZERO];
      X_GEN_TAC `x:P` THEN
      MATCH_MP_TAC N_INDUCT THEN
      REPEAT CONJ_TAC THENL
      [X_GEN_TAC `y:P` THEN
       MATCH_MP_TAC N_INDUCT THEN
       REPEAT CONJ_TAC THENL
       [X_GEN_TAC `z:P` THEN
        REWRITE_TAC [mult_def; N_INJECTIVE] THEN
        MESON_TAC [MULTP_ASSOC];
        REWRITE_TAC [MULT_LZERO; MULT_RZERO];
        X_GEN_TAC `z:P` THEN
        REWRITE_TAC [mult_def; N_INJECTIVE] THEN
        MESON_TAC [MULTP_ASSOC]];
       REWRITE_TAC [MULT_LZERO; MULT_RZERO];
       X_GEN_TAC `y:P` THEN
       MATCH_MP_TAC N_INDUCT THEN
       REPEAT CONJ_TAC THENL
       [X_GEN_TAC `z:P` THEN
        REWRITE_TAC [mult_def; N_INJECTIVE] THEN
        MESON_TAC [MULTP_ASSOC];
        REWRITE_TAC [MULT_LZERO; MULT_RZERO];
        X_GEN_TAC `z:P` THEN
        REWRITE_TAC [mult_def; N_INJECTIVE] THEN
        MESON_TAC [MULTP_ASSOC]]]]));;
