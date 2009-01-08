(* For interactive use
let lemma x = x ();;
let theorem x = x ();;
*)

(* Helper functions *)

let remove_first x =
    let rec remove seen unseen =
        match unseen with
          [] -> failwith "remove_first"
        | x' :: unseen ->
          if x = x' then List.rev seen @ unseen
          else remove (x' :: seen) unseen
    in remove [];;

let term_size =
    let rec sz acc tms =
        match tms with
          [] -> acc
        | tm :: tms ->
          if is_comb tm then
            let (x,y) = dest_comb tm in
            sz (acc + 1) (x :: y :: tms)
          else if is_abs tm then
            let (_,x) = dest_abs tm in
            sz (acc + 1) (x :: tms)
          else sz acc tms
    in fun tm -> sz 0 [tm];;

let term_size_compare tm1 tm2 = term_size tm1 <= term_size tm2;;

let MK_EQ = lemma (fun () -> MESON []
    `(x1 = y1) /\ (x2 = y2) ==> ((x1 = x2) <=> (y1 = y2))`);;

let CCONTR_TAC =
    let neg_neg = lemma (fun () -> MESON [] `~ ~x ==> x`) in
    let neg_neg_neg = lemma (fun () -> MESON [] `~x ==> ~ ~ ~x`) in
    let neg_true = lemma (fun () -> MESON [] `F ==> ~T`) in
    let neg_neg_false = lemma (fun () -> MESON [] `F ==> ~ ~F`) in
    MATCH_MP_TAC neg_neg THEN
    REPEAT (MATCH_MP_TAC neg_neg_neg) THEN
    (MATCH_MP_TAC neg_true ORELSE
     MATCH_MP_TAC neg_neg_false ORELSE
     STRIP_TAC);;

(* The existing type of positive numbers *)

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

let ADDP_MONO = new_axiom
    (`!x x' y y'. leP x x' /\ leP y y' ==> leP (addP x y) (addP x' y')`);;

let ADDP_INC = new_axiom (`!x y. ~leP (addP x y) x`);;

let SUBP_ELIM = new_axiom (`!x y. ~leP y x ==> addP x (subP y x) = y`);;

let MULTP_COMM = new_axiom (`!x y. multP x y = multP y x`);;

let MULTP_ASSOC = new_axiom
    (`!x y z. multP (multP x y) z = multP x (multP y z)`);;

let LEP_REFL = new_axiom (`!x. leP x x`);;

let LEP_TRANS = new_axiom (`!x y z. leP x y /\ leP y z ==> leP x z`);;

let LEP_ANTISYM = new_axiom (`!x y. leP x y /\ leP y x ==> x = y`);;

let LEP_TOTAL = new_axiom (`!x y. leP x y \/ leP y x`);;

(* Lemmas about positive numbers *)

let LEP_TRICH = lemma (fun () -> prove
    (`!x y.
        x = y \/
        (leP x y /\ ~(leP y x)) \/
        (~(leP x y) /\ leP y x)`,
     MESON_TAC [LEP_ANTISYM; LEP_TOTAL]));;

let LEP_TOTAL_IMP = lemma (fun () -> prove
    (`!x y. ~leP x y ==> leP y x`,
     MESON_TAC [LEP_TOTAL]));;

let ADDP_INC' = lemma (fun () -> ONCE_REWRITE_RULE [ADDP_COMM] ADDP_INC);;

let ADDP_NEQ = lemma (fun () -> prove
    (`!x y. ~(addP x y = x)`,
     MESON_TAC [ADDP_INC; LEP_REFL]));;

let ADDP_NEQ' = lemma (fun () -> ONCE_REWRITE_RULE [ADDP_COMM] ADDP_NEQ);;

let ADDP_LEP_CANCEL_IMP = lemma (fun () -> prove
    (`!x y z. leP (addP x y) (addP x z) ==> leP y z`,
     REPEAT STRIP_TAC THEN
     MATCH_MP_TAC (MESON [] `(~x ==> F) ==> x`) THEN
     STRIP_TAC THEN
     MP_TAC (SPECL [`addP x z`; `subP y z`] ADDP_INC) THEN
     ASM_REWRITE_TAC [ADDP_ASSOC] THEN
     ASM_MESON_TAC [SUBP_ELIM; LEP_REFL]));;

let ADDP_LEP_CANCEL_IMP' = lemma (fun () -> prove
    (`!x y z. leP y z ==> leP (addP x y) (addP x z)`,
     REPEAT STRIP_TAC THEN
     ASM_MESON_TAC [ADDP_MONO; LEP_REFL]));;

let ADDP_LEP_CANCEL = lemma (fun () -> prove
    (`!x y z. leP (addP x y) (addP x z) <=> leP y z`,
     MESON_TAC [ADDP_LEP_CANCEL_IMP; ADDP_LEP_CANCEL_IMP']));;

let ADDP_CANCEL_IMP = lemma (fun () -> prove
    (`!x y z. addP x y = addP x z ==> y = z`,
     REPEAT STRIP_TAC THEN
     MATCH_MP_TAC LEP_ANTISYM THEN
     ASM_MESON_TAC [ADDP_LEP_CANCEL_IMP; LEP_REFL]));;

let ADDP_CANCEL = lemma (fun () -> prove
    (`!x y z. (addP x y = addP x z) <=> y = z`,
     MESON_TAC [ADDP_CANCEL_IMP]));;

let ADDP_LEP_IMP = lemma (fun () -> prove
    (`!x y z. leP (addP x y) z ==> ~leP z x`,
     REPEAT STRIP_TAC THEN
     MP_TAC (SPECL [`x:P`; `y:P`] ADDP_INC) THEN
     REWRITE_TAC [] THEN
     MATCH_MP_TAC LEP_TRANS THEN
     EXISTS_TAC `z:P` THEN
     ASM_REWRITE_TAC []));;

let ADDP_LEP_IMP' = lemma (fun () -> prove
    (`!x y z. leP (addP x y) z ==> ~leP z y`,
     MESON_TAC [ADDP_LEP_IMP; ADDP_COMM]));;

let SUBP_REQ_IMP = lemma (fun () -> prove
    (`!x y z. ~leP x y /\ addP z y = x ==> z = subP x y`,
     REPEAT STRIP_TAC THEN
     MATCH_MP_TAC ADDP_CANCEL_IMP THEN
     EXISTS_TAC `y:P` THEN
     ASM_SIMP_TAC [SUBP_ELIM] THEN
     ASM_MESON_TAC [ADDP_COMM]));;

let SUBP_REQ_ELIM = lemma (fun () -> prove
    (`!x y z. ~leP x y ==> (z = subP x y <=> addP z y = x)`,
     REPEAT STRIP_TAC THEN
     EQ_TAC THENL
     [DISCH_THEN (fun th -> REWRITE_TAC [th]) THEN
      ONCE_REWRITE_TAC [ADDP_COMM] THEN
      ASM_SIMP_TAC [SUBP_ELIM];
      ASM_MESON_TAC [SUBP_REQ_IMP]]));;

let SUBP_LEQ_IMP = lemma (fun () -> prove
    (`!x y z. ~leP x y /\ x = addP z y ==> subP x y = z`,
     MESON_TAC [SUBP_REQ_IMP]));;

let SUBP_LEQ_ELIM = lemma (fun () -> prove
    (`!x y z. ~leP x y ==> (subP x y = z <=> x = addP z y)`,
     MESON_TAC [SUBP_REQ_ELIM]));;

let SUBP_ADDP_REQ_IMP = lemma (fun () -> prove
    (`!x y z w. ~leP x y /\ addP z y = addP x w ==> z = addP (subP x y) w`,
     REPEAT STRIP_TAC THEN
     MATCH_MP_TAC ADDP_CANCEL_IMP THEN
     EXISTS_TAC `y:P` THEN
     REWRITE_TAC [GSYM ADDP_ASSOC] THEN
     ASM_SIMP_TAC [SUBP_ELIM] THEN
     ASM_MESON_TAC [ADDP_COMM]));;

let SUBP_ADDP_LEQ_IMP = lemma (fun () -> prove
    (`!x y z w. ~leP x y /\ addP x w = addP z y ==> addP (subP x y) w = z`,
     MESON_TAC [SUBP_ADDP_REQ_IMP]));;

let SUBP_LLEP_IMP = lemma (fun () -> prove
    (`!x y z. ~leP x y /\ leP (subP x y) z ==> leP x (addP z y)`,
     REPEAT STRIP_TAC THEN
     ONCE_REWRITE_TAC [ADDP_COMM] THEN
     MP_TAC (SPECL [`y:P`; `x:P`] SUBP_ELIM) THEN
     ASM_REWRITE_TAC [] THEN
     DISCH_THEN (fun th -> ONCE_REWRITE_TAC [GSYM th]) THEN
     ASM_REWRITE_TAC [ADDP_LEP_CANCEL]));;

let SUBP_LLEP_IMP' = lemma (fun () -> prove
    (`!x y z. ~leP x y /\ leP x (addP z y) ==> leP (subP x y) z`,
     ONCE_REWRITE_TAC [ADDP_COMM] THEN
     REPEAT STRIP_TAC THEN
     MATCH_MP_TAC ADDP_LEP_CANCEL_IMP THEN
     EXISTS_TAC `y:P` THEN
     ASM_SIMP_TAC [SUBP_ELIM]));;

let SUBP_LLEP = lemma (fun () -> prove
    (`!x y z. ~leP x y ==> (leP (subP x y) z <=> leP x (addP z y))`,
     MESON_TAC [SUBP_LLEP_IMP; SUBP_LLEP_IMP']));;

let SUBP_RLEP_IMP = lemma (fun () -> prove
    (`!x y z. ~leP x y /\ leP z (subP x y) ==> leP (addP z y) x`,
     REPEAT STRIP_TAC THEN
     ONCE_REWRITE_TAC [ADDP_COMM] THEN
     MP_TAC (SPECL [`y:P`; `x:P`] SUBP_ELIM) THEN
     ASM_REWRITE_TAC [] THEN
     DISCH_THEN (fun th -> ONCE_REWRITE_TAC [GSYM th]) THEN
     ASM_REWRITE_TAC [ADDP_LEP_CANCEL]));;

let SUBP_RLEP_IMP' = lemma (fun () -> prove
    (`!x y z. ~leP x y /\ leP (addP z y) x ==> leP z (subP x y)`,
     ONCE_REWRITE_TAC [ADDP_COMM] THEN
     REPEAT STRIP_TAC THEN
     MATCH_MP_TAC ADDP_LEP_CANCEL_IMP THEN
     EXISTS_TAC `y:P` THEN
     ASM_SIMP_TAC [SUBP_ELIM]));;

let SUBP_RLEP = lemma (fun () -> prove
    (`!x y z. ~leP x y ==> (leP z (subP x y) <=> leP (addP z y) x)`,
     MESON_TAC [SUBP_RLEP_IMP; SUBP_RLEP_IMP']));;

let SUBP_DEC = lemma (fun () -> prove
    (`!x y. ~leP x y ==> ~leP x (subP x y)`,
     SIMP_TAC [SUBP_RLEP; ADDP_INC]));;

let SUBP_NEQ = lemma (fun () -> prove
    (`!x y. ~leP x y ==> ~(subP x y = x)`,
     MESON_TAC [SUBP_DEC; LEP_REFL]));;

let SUBP_ADDP_RLEP_IMP = lemma (fun () -> prove
    (`!x y z w.
        ~leP x y /\ leP z (addP (subP x y) w) ==> leP (addP z y) (addP x w)`,
     REPEAT STRIP_TAC THEN
     MP_TAC (SPECL [`y:P`; `x:P`] SUBP_ELIM) THEN
     ASM_REWRITE_TAC [] THEN
     DISCH_THEN (fun th -> ONCE_REWRITE_TAC [GSYM th]) THEN
     CONV_TAC (LAND_CONV (ONCE_REWRITE_CONV [ADDP_COMM])) THEN
     ASM_REWRITE_TAC [ADDP_ASSOC; ADDP_LEP_CANCEL]));;

let SUBP_ADDP_RLEP_IMP' = lemma (fun () -> prove
    (`!x y z w.
        ~leP x y /\ leP (addP z y) (addP x w) ==> leP z (addP (subP x y) w)`,
     REPEAT STRIP_TAC THEN
     MATCH_MP_TAC ADDP_LEP_CANCEL_IMP THEN
     EXISTS_TAC `y:P` THEN
     REWRITE_TAC [GSYM ADDP_ASSOC] THEN
     ASM_SIMP_TAC [SUBP_ELIM] THEN
     ASM_MESON_TAC [ADDP_COMM]));;

let SUBP_ADDP_RLEP = lemma (fun () -> prove
    (`!x y z w.
        ~leP x y ==> (leP z (addP (subP x y) w) <=> leP (addP z y) (addP x w))`,
     MESON_TAC [SUBP_ADDP_RLEP_IMP; SUBP_ADDP_RLEP_IMP']));;

(* Proof tools for positive numbers *)

let dest_lep = dest_binop `leP`;;

let dest_cond_lep tm =
    let (c,xy) = dest_cond tm in
    (dest_lep c, xy);;

let is_cond_lep = can dest_cond_lep;;

let mk_addp = mk_binop `addP`;;

let rec list_mk_addp tms =
    match tms with
      [] -> failwith "list_mk_addp"
    | tm :: tms ->
      if length tms = 0 then tm else mk_addp tm (list_mk_addp tms);;

let dest_addp = dest_binop `addP`;;

let is_addp = can dest_addp;;

let rec strip_addp tm =
    if is_addp tm then
      let (x,y) = dest_addp tm in
      strip_addp x @ strip_addp y
    else [tm];;

let addp_ac = AC (lemma (fun () -> MESON [ADDP_COMM; ADDP_ASSOC]
    `addP m n = addP n m /\
     addP (addP m n) p = addP m (addP n p) /\
     addP m (addP n p) = addP n (addP m p)`));;

let dest_subp = dest_binop `subP`;;

let is_subp = can dest_subp;;

let rec TRICH_TAC x y =
    if x = y then ASM_REWRITE_TAC [LEP_REFL]
    else if can (find_term ((=) x)) y then TRICH_TAC y x
    else
      MP_TAC (SPECL [x; y] LEP_TRICH) THEN
      STRIP_TAC THEN
      ASM_REWRITE_TAC [LEP_REFL];;

let LEP_TAC goal =
    let tm = find_term is_cond_lep (snd goal) in
    let ((x,y),_) = dest_cond_lep tm in
    TRICH_TAC x y goal;;

let SUB_ELIM_CONV tm =
    let tms = strip_addp tm in
    let rec elim stms =
        match stms with
          [] -> failwith "SUB_ELIM_CONV"
        | stm :: stms ->
          if not (is_subp stm) then elim stms else
          let (x,y) = dest_subp stm in
          if not (mem y tms) then elim stms else
          let tms = remove_first stm tms in
          let tms = remove_first y tms in
          let etm = mk_addp y stm in
          let tm' = if length tms = 0 then etm
                    else mk_addp etm (list_mk_addp tms) in
          addp_ac (mk_eq (tm,tm')) in
    elim tms;;

let SUB_MOVE_CONV tm =
    let tms = strip_addp tm in
    let rec move stms =
        match stms with
          [] -> failwith "SUB_MOVE_CONV"
        | stm :: stms ->
          if not (is_subp stm) then move stms else
          let tms = remove_first stm tms in
          let tm' = list_mk_addp (stm :: tms) in
          addp_ac (mk_eq (tm,tm')) in
    move tms;;

let ADD_CANCEL_CONV tm =
    let (tm1,tm2) = dest_eq tm in
    let tms1 = strip_addp tm1 in
    let tms2 = strip_addp tm2 in
    let rec cancel stms =
        match stms with
          [] -> failwith "ADD_CANCEL_CONV"
        | stm :: stms ->
          if not (mem stm tms2) then cancel stms else
          let tms1 = stm :: remove_first stm tms1 in
          let tms2 = stm :: remove_first stm tms2 in
          let tm1' = list_mk_addp tms1 in
          let tm2' = list_mk_addp tms2 in
          let th1 = addp_ac (mk_eq (tm1,tm1')) in
          let th2 = addp_ac (mk_eq (tm2,tm2')) in
          MATCH_MP MK_EQ (CONJ th1 th2) in
    cancel tms1;;

let LEP_SUBP_ELIM_TAC =
    ASSUM_LIST
      (fun ths ->
           let ths = map concl ths in
           let ths = rev (sort term_size_compare ths) in
           let sub_elim_conv =
               LAND_CONV SUB_ELIM_CONV ORELSEC
               RAND_CONV SUB_ELIM_CONV in
           EVERY (map (fun tm ->
              UNDISCH_TAC tm THEN
              ASM_SIMP_TAC [SUBP_LLEP; SUBP_RLEP] THEN
              REPEAT
                (CONV_TAC
                   (sub_elim_conv ORELSEC
                    RAND_CONV sub_elim_conv ORELSEC
                    LAND_CONV sub_elim_conv ORELSEC
                    LAND_CONV (RAND_CONV sub_elim_conv)) THEN
                 ASM_SIMP_TAC [SUBP_ELIM])) ths));;

let MATCH_MP_LIST mps th =
    let rec infer ths mps =
        match mps with
          [] -> rev ths
        | mp :: mps ->
          let ths = try MATCH_MP mp th :: ths with Failure _ -> ths in
          infer ths mps in
    infer [] mps;;

let REPEAT_MATCH_MP_LIST mps =
    let rec infer ths tms inp =
        match inp with
          [] -> rev ths
        | th :: inp ->
          let tm = concl th in
          if mem tm tms then infer ths tms inp else
          let ths = th :: ths in
          let tms = tm :: tms in
          let inp = MATCH_MP_LIST mps th @ inp in
          infer ths tms inp
    in infer [] [];;

let LEP_ADDP_TAC =
    let ths = [LEP_TOTAL_IMP; ADDP_LEP_IMP; ADDP_LEP_IMP'] in
    POP_ASSUM_LIST (EVERY o map ASSUME_TAC o REPEAT_MATCH_MP_LIST ths);;

(* The new type of numbers *)

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

(* Theorems of the new type of numbers *)

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

let ADD_ASSOC = theorem (fun () -> prove
    (`!x y z. add (add x y) z = add x (add y z)`,
     MATCH_MP_TAC N_INDUCT THEN
     REPEAT CONJ_TAC THEN
     (X_GEN_TAC `x:P` ORELSE REWRITE_TAC [ADD_LZERO; ADD_RZERO]) THEN
     MATCH_MP_TAC N_INDUCT THEN
     REPEAT CONJ_TAC THEN
     (X_GEN_TAC `y:P` ORELSE REWRITE_TAC [ADD_LZERO; ADD_RZERO]) THEN
     MATCH_MP_TAC N_INDUCT THEN
     REPEAT CONJ_TAC THEN
     (X_GEN_TAC `z:P` ORELSE REWRITE_TAC [ADD_LZERO; ADD_RZERO]) THEN
     ASM_REWRITE_TAC [add_def; N_INJECTIVE; N_DISTINCT] THEN
     REPEAT LEP_TAC THEN
     ASM_REWRITE_TAC [add_def; N_INJECTIVE; N_DISTINCT] THEN
     REPEAT LEP_TAC THEN
     ASM_REWRITE_TAC [N_INJECTIVE; N_DISTINCT; ADDP_ASSOC] THEN
     REPEAT (FIRST_X_ASSUM SUBST_VAR_TAC) THEN
     REPEAT
       (((MATCH_MP_TAC SUBP_LEQ_IMP ORELSE
          MATCH_MP_TAC SUBP_REQ_IMP) THEN
         ASM_REWRITE_TAC []) ORELSE
        (CONV_TAC (LAND_CONV SUB_MOVE_CONV) THEN
         MATCH_MP_TAC SUBP_ADDP_LEQ_IMP THEN
         ASM_REWRITE_TAC []) ORELSE
        (CONV_TAC (RAND_CONV SUB_MOVE_CONV) THEN
         MATCH_MP_TAC SUBP_ADDP_REQ_IMP THEN
         ASM_REWRITE_TAC []) ORELSE
        REFL_TAC ORELSE
        CONV_TAC (ADD_CANCEL_CONV THENC REWR_CONV ADDP_CANCEL)) THEN
     LEP_SUBP_ELIM_TAC THEN
     SIMP_TAC [ADDP_INC; ADDP_INC'; ADDP_NEQ; ADDP_NEQ'; SUBP_NEQ] THEN
     REPEAT STRIP_TAC THEN
     CCONTR_TAC THEN
     LEP_ADDP_TAC THEN
     ASM_MESON_TAC [LEP_ANTISYM; LEP_REFL; ADDP_COMM]));;

let ADD_LE_CANCEL_IMP = lemma (fun () -> prove
    (`!x y z. le (add x y) (add x z) ==> le y z`,
     MATCH_MP_TAC N_INDUCT THEN
     REPEAT CONJ_TAC THEN
     (X_GEN_TAC `x:P` ORELSE REWRITE_TAC [ADD_LZERO; ADD_RZERO]) THEN
     MATCH_MP_TAC N_INDUCT THEN
     REPEAT CONJ_TAC THEN
     (X_GEN_TAC `y:P` ORELSE REWRITE_TAC [ADD_LZERO; ADD_RZERO]) THEN
     MATCH_MP_TAC N_INDUCT THEN
     REPEAT CONJ_TAC THEN
     (X_GEN_TAC `z:P` ORELSE REWRITE_TAC [ADD_LZERO; ADD_RZERO]) THEN
     ASM_REWRITE_TAC [add_def; le_def; N_INJECTIVE; N_DISTINCT] THEN
     REPEAT LEP_TAC THEN
     ASM_REWRITE_TAC [add_def; le_def; N_INJECTIVE; N_DISTINCT] THEN
     REPEAT LEP_TAC THEN
     REPEAT STRIP_TAC THEN
     LEP_SUBP_ELIM_TAC THEN
     SIMP_TAC
       [ADDP_INC; ADDP_INC'; ADDP_ASSOC; ADDP_LEP_CANCEL; SUBP_ADDP_RLEP] THEN
     REPEAT STRIP_TAC THEN
     CCONTR_TAC THEN
     LEP_ADDP_TAC THEN
     ASM_MESON_TAC [LEP_ANTISYM; LEP_REFL; LEP_TRANS]));;

let ADD_LE_CANCEL_IMP' = lemma (fun () -> prove
    (`!x y z. le y z ==> le (add x y) (add x z)`,
     REPEAT STRIP_TAC THEN
     MATCH_MP_TAC ADD_LE_CANCEL_IMP THEN
     EXISTS_TAC `neg x` THEN
     ASM_REWRITE_TAC [GSYM ADD_ASSOC; ADD_LNEG; ADD_LZERO]));;

let ADD_LE_CANCEL = theorem (fun () -> prove
    (`!x y z. le (add x y) (add x z) <=> le y z`,
     MESON_TAC [ADD_LE_CANCEL_IMP; ADD_LE_CANCEL_IMP']));;

let ADD_CANCEL = theorem (fun () -> prove
    (`!x y z. add x y = add x z <=> y = z`,
     MESON_TAC [ADD_LE_CANCEL; LE_REFL; LE_ANTISYM]));;

let ADD_MONO = theorem (fun () -> prove
    (`!x x' y y'. le x x' /\ le y y' ==> le (add x y) (add x' y')`,
     REPEAT STRIP_TAC THEN
     MATCH_MP_TAC LE_TRANS THEN
     EXISTS_TAC `add x' y` THEN
     STRIP_TAC THENL
     [ONCE_REWRITE_TAC [ADD_COMM] THEN
      ASM_REWRITE_TAC [ADD_LE_CANCEL];
      ASM_REWRITE_TAC [ADD_LE_CANCEL]]));;

let SUB_ELIM = theorem (fun () -> prove
    (`!x y. add x (sub y x) = y`,
     REWRITE_TAC [sub_def] THEN
     REPEAT GEN_TAC THEN
     CONV_TAC (LAND_CONV (RAND_CONV (REWR_CONV ADD_COMM))) THEN
     REWRITE_TAC [GSYM ADD_ASSOC; ADD_RNEG; ADD_LZERO]));;

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
