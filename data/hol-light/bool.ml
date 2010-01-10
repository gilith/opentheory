(* ========================================================================= *)
(* Boolean theory including (intuitionistic) defs of logical connectives.    *)
(*                                                                           *)
(*       John Harrison, University of Cambridge Computer Laboratory          *)
(*                                                                           *)
(*            (c) Copyright, University of Cambridge 1998                    *)
(*              (c) Copyright, John Harrison 1998-2007                       *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* Set up parse status of basic and derived logical constants.               *)
(* ------------------------------------------------------------------------- *)

parse_as_prefix "~";;

map parse_as_binder ["\\"; "!"; "?"; "?!"];;

map parse_as_infix ["==>",(4,"right"); "\\/",(6,"right"); "/\\",(8,"right")];;

(* ------------------------------------------------------------------------- *)
(* Set up more orthodox notation for equations and equivalence.              *)
(* ------------------------------------------------------------------------- *)

parse_as_infix("<=>",(2,"right"));;
override_interface ("<=>",`(=):bool->bool->bool`);;
parse_as_infix("=",(12,"right"));;

(* ------------------------------------------------------------------------- *)
(* Special syntax for Boolean equations (IFF).                               *)
(* ------------------------------------------------------------------------- *)

let is_iff tm =
  match tm with
    Comb(Comb(Const("=",Tyapp("fun",[Tyapp("bool",[]);_])),l),r) -> true
  | _ -> false;;

let dest_iff tm =
  match tm with
    Comb(Comb(Const("=",Tyapp("fun",[Tyapp("bool",[]);_])),l),r) -> (l,r)
  | _ -> failwith "dest_iff";;

let mk_iff =
  let eq_tm = `(<=>)` in
  fun (l,r) -> mk_comb(mk_comb(eq_tm,l),r);;

(* ------------------------------------------------------------------------- *)
(* Log the definitions as a theory.                                          *)
(* ------------------------------------------------------------------------- *)

logfile "bool-def";;

(* ------------------------------------------------------------------------- *)
(* Rules for T                                                               *)
(* ------------------------------------------------------------------------- *)

let T_DEF = new_basic_definition
 `T = ((\p:bool. p) = (\p:bool. p))`;;

(* ------------------------------------------------------------------------- *)
(* Rules for /\                                                              *)
(* ------------------------------------------------------------------------- *)

let AND_DEF = new_basic_definition
 `(/\) = \p q. (\f:bool->bool->bool. f p q) = (\f. f T T)`;;

let mk_conj = mk_binary "/\\";;
let list_mk_conj = end_itlist (curry mk_conj);;

(* ------------------------------------------------------------------------- *)
(* Rules for ==>                                                             *)
(* ------------------------------------------------------------------------- *)

let IMP_DEF = new_basic_definition
  `(==>) = \p q. p /\ q <=> p`;;

let mk_imp = mk_binary "==>";;

(* ------------------------------------------------------------------------- *)
(* Rules for !                                                               *)
(* ------------------------------------------------------------------------- *)

let FORALL_DEF = new_basic_definition
 `(!) = \P:A->bool. P = \x. T`;;

let mk_forall = mk_binder "!";;
let list_mk_forall(vs,bod) = itlist (curry mk_forall) vs bod;;

(* ------------------------------------------------------------------------- *)
(* Rules for ?                                                               *)
(* ------------------------------------------------------------------------- *)

let EXISTS_DEF = new_basic_definition
 `(?) = \P:A->bool. !q. (!x. P x ==> q) ==> q`;;

let mk_exists =  mk_binder "?";;
let list_mk_exists(vs,bod) =  itlist (curry mk_exists) vs bod;;

(* ------------------------------------------------------------------------- *)
(* Rules for \/                                                              *)
(* ------------------------------------------------------------------------- *)

let OR_DEF = new_basic_definition
 `(\/) = \p q. !r. (p ==> r) ==> (q ==> r) ==> r`;;

let mk_disj = mk_binary "\\/";;
let list_mk_disj = end_itlist (curry mk_disj);;

(* ------------------------------------------------------------------------- *)
(* Rules for negation and falsity.                                           *)
(* ------------------------------------------------------------------------- *)

let F_DEF = new_basic_definition
 `F = !p:bool. p`;;

let NOT_DEF = new_basic_definition
 `(~) = \p. p ==> F`;;

let mk_neg =
  let neg_tm = `(~)` in
  fun tm -> try mk_comb(neg_tm,tm)
            with Failure _ -> failwith "mk_neg";;

(* ------------------------------------------------------------------------- *)
(* Rules for unique existence.                                               *)
(* ------------------------------------------------------------------------- *)

let EXISTS_UNIQUE_DEF = new_basic_definition
 `(?!) = \P:A->bool. ((?) P) /\ (!x y. P x /\ P y ==> x = y)`;;

let mk_uexists = mk_binder "?!";;

(* ------------------------------------------------------------------------- *)
(* Log the rule theorems as another theory.                                  *)
(* ------------------------------------------------------------------------- *)

logfile "bool-thm";;

(* ------------------------------------------------------------------------- *)
(* Rule allowing easy instantiation of polymorphic proformas.                *)
(* ------------------------------------------------------------------------- *)

let PINST tyin tmin =
  let iterm_fn = INST (map (I F_F (inst tyin)) tmin)
  and itype_fn = INST_TYPE tyin in
  fun th -> try iterm_fn (itype_fn th)
            with Failure _ -> failwith "PINST";;

let PINST = log_function3 "PINST" log_type_inst log_inst log_thm log_thm PINST;;

(* ------------------------------------------------------------------------- *)
(* Useful derived deductive rule.                                            *)
(* ------------------------------------------------------------------------- *)

let PROVE_HYP ath bth =
  if exists (aconv (concl ath)) (hyp bth)
  then EQ_MP (DEDUCT_ANTISYM_RULE ath bth) ath
  else bth;;

let PROVE_HYP = log_function2 "PROVE_HYP" log_thm log_thm log_thm PROVE_HYP;;

(* ------------------------------------------------------------------------- *)
(* Rules for T                                                               *)
(* ------------------------------------------------------------------------- *)

let TRUTH = EQ_MP (SYM T_DEF) (REFL `\p:bool. p`);;

let EQT_ELIM th =
  try EQ_MP (SYM th) TRUTH
  with Failure _ -> failwith "EQT_ELIM";;

let EQT_ELIM = log_function "EQT_ELIM" log_thm log_thm EQT_ELIM;;

let EQT_INTRO =
  let t = `t:bool` in
  let pth =
    log_lemma "EQT_INTRO.pth" (fun () ->
    let th1 = DEDUCT_ANTISYM_RULE (ASSUME t) TRUTH in
    let th2 = EQT_ELIM(ASSUME(concl th1)) in
    DEDUCT_ANTISYM_RULE th2 th1) in
  fun th -> EQ_MP (INST[concl th,t] pth) th;;

let EQT_INTRO = log_function "EQT_INTRO" log_thm log_thm EQT_INTRO;;

(* ------------------------------------------------------------------------- *)
(* Rules for /\                                                              *)
(* ------------------------------------------------------------------------- *)

let CONJ =
  let f = `f:bool->bool->bool`
  and p = `p:bool`
  and q = `q:bool` in
  let pth =
    log_lemma "CONJ.pth" (fun () ->
    let pth = ASSUME p
    and qth = ASSUME q in
    let th1 = MK_COMB(AP_TERM f (EQT_INTRO pth),EQT_INTRO qth) in
    let th2 = ABS f th1 in
    let th3 = BETA_RULE (AP_THM (AP_THM AND_DEF p) q) in
    EQ_MP (SYM th3) th2) in
  fun th1 th2 ->
    let th = INST [concl th1,p; concl th2,q] pth in
    PROVE_HYP th2 (PROVE_HYP th1 th);;

let CONJ = log_function2 "CONJ" log_thm log_thm log_thm CONJ;;

let CONJUNCT1 =
  let P = `P:bool` and Q = `Q:bool` in
  let pth =
    log_lemma "CONJUNCT1.pth" (fun () ->
    let th1 = CONV_RULE (RAND_CONV BETA_CONV) (AP_THM AND_DEF `P:bool`) in
    let th2 = CONV_RULE (RAND_CONV BETA_CONV) (AP_THM th1 `Q:bool`) in
    let th3 = EQ_MP th2 (ASSUME `P /\ Q`) in
    EQT_ELIM(BETA_RULE (AP_THM th3 `\(p:bool) (q:bool). p`))) in
  fun th ->
    try let l,r = dest_conj(concl th) in
        PROVE_HYP th (INST [l,P; r,Q] pth)
    with Failure _ -> failwith "CONJUNCT1";;

let CONJUNCT1 = log_function "CONJUNCT1" log_thm log_thm CONJUNCT1;;

let CONJUNCT2 =
  let P = `P:bool` and Q = `Q:bool` in
  let pth =
    log_lemma "CONJUNCT2.pth" (fun () ->
    let th1 = CONV_RULE (RAND_CONV BETA_CONV) (AP_THM AND_DEF `P:bool`) in
    let th2 = CONV_RULE (RAND_CONV BETA_CONV) (AP_THM th1 `Q:bool`) in
    let th3 = EQ_MP th2 (ASSUME `P /\ Q`) in
    EQT_ELIM(BETA_RULE (AP_THM th3 `\(p:bool) (q:bool). q`))) in
  fun th ->
    try let l,r = dest_conj(concl th) in
        PROVE_HYP th (INST [l,P; r,Q] pth)
    with Failure _ -> failwith "CONJUNCT2";;

let CONJUNCT2 = log_function "CONJUNCT2" log_thm log_thm CONJUNCT2;;

let CONJ_PAIR th =
  try CONJUNCT1 th,CONJUNCT2 th
  with Failure _ -> failwith "CONJ_PAIR: Not a conjunction";;

let CONJ_PAIR =
    log_function "CONJ_PAIR" log_thm (log_pair log_thm log_thm) CONJ_PAIR;;

let CONJUNCTS = striplist CONJ_PAIR;;

let CONJUNCTS = log_function "CONJUNCTS" log_thm (log_list log_thm) CONJUNCTS;;

(* ------------------------------------------------------------------------- *)
(* Rules for ==>                                                             *)
(* ------------------------------------------------------------------------- *)

let MP =
  let p = `p:bool`
  and q = `q:bool` in
  let pth =
    log_lemma "MP.pth" (fun () ->
    let th1 = BETA_RULE (AP_THM (AP_THM IMP_DEF p) q) in
    let th2 = EQ_MP th1 (ASSUME `p ==> q`) in
    CONJUNCT2 (EQ_MP (SYM th2) (ASSUME `p:bool`))) in
  fun ith th ->
    let ant,con = dest_imp (concl ith) in
    if aconv ant (concl th) then
      PROVE_HYP th (PROVE_HYP ith (INST [ant,p; con,q] pth))
    else failwith "MP: theorems do not agree";;

let MP = log_function2 "MP" log_thm log_thm log_thm MP;;

let DISCH =
  let p = `p:bool`
  and q = `q:bool` in
  let pth =
    log_lemma "DISCH.pth" (fun () ->
    SYM(BETA_RULE (AP_THM (AP_THM IMP_DEF p) q))) in
  fun a th ->
    let th1 = CONJ (ASSUME a) th in
    let th2 = CONJUNCT1 (ASSUME (concl th1)) in
    let th3 = DEDUCT_ANTISYM_RULE th1 th2 in
    let th4 = INST [a,p; concl th,q] pth in
    EQ_MP th4 th3;;

let DISCH = log_function2 "DISCH" log_term log_thm log_thm DISCH;;

let rec DISCH_ALL th =
  try DISCH_ALL (DISCH (hd (hyp th)) th)
  with Failure _ -> th;;

let DISCH_ALL = log_function "DISCH_ALL" log_thm log_thm DISCH_ALL;;

let UNDISCH th =
  try MP th (ASSUME(rand(rator(concl th))))
  with Failure _ -> failwith "UNDISCH";;

let UNDISCH = log_function "UNDISCH" log_thm log_thm UNDISCH;;

let rec UNDISCH_ALL th =
  if is_imp (concl th) then UNDISCH_ALL (UNDISCH th)
  else th;;

let UNDISCH_ALL = log_function "UNDISCH_ALL" log_thm log_thm UNDISCH_ALL;;

let IMP_ANTISYM_RULE th1 th2 =
  DEDUCT_ANTISYM_RULE (UNDISCH th2) (UNDISCH th1);;

let IMP_ANTISYM_RULE =
    log_function2 "IMP_ANTISYM_RULE" log_thm log_thm log_thm IMP_ANTISYM_RULE;;

let ADD_ASSUM tm th = MP (DISCH tm th) (ASSUME tm);;

let ADD_ASSUM = log_function2 "ADD_ASSUM" log_term log_thm log_thm ADD_ASSUM;;

let EQ_IMP_RULE =
  let peq = `p <=> q` in
  let p,q = dest_iff peq in
  let pth1 =
      log_lemma "EQ_IMP_RULE.pth1" (fun () ->
      DISCH peq (DISCH p (EQ_MP (ASSUME peq) (ASSUME p))))
  and pth2 =
      log_lemma "EQ_IMP_RULE.pth2" (fun () ->
      DISCH peq (DISCH q (EQ_MP (SYM(ASSUME peq)) (ASSUME q)))) in
  fun th -> let l,r = dest_iff(concl th) in
            MP (INST [l,p; r,q] pth1) th,MP (INST [l,p; r,q] pth2) th;;

let EQ_IMP_RULE =
    log_function "EQ_IMP_RULE" log_thm (log_pair log_thm log_thm) EQ_IMP_RULE;;

let IMP_TRANS =
  let pq = `p ==> q`
  and qr = `q ==> r` in
  let p,q = dest_imp pq and r = rand qr in
  let pth =
      log_lemma "IMP_TRANS.pth" (fun () ->
      itlist DISCH [pq; qr; p] (MP (ASSUME qr) (MP (ASSUME pq) (ASSUME p)))) in
  fun th1 th2 ->
        let x,y = dest_imp(concl th1)
        and y',z = dest_imp(concl th2) in
        if y <> y' then failwith "IMP_TRANS" else
        MP (MP (INST [x,p; y,q; z,r] pth) th1) th2;;

let IMP_TRANS = log_function2 "IMP_TRANS" log_thm log_thm log_thm IMP_TRANS;;

(* ------------------------------------------------------------------------- *)
(* Rules for !                                                               *)
(* ------------------------------------------------------------------------- *)

let SPEC =
  let P = `P:A->bool`
  and x = `x:A` in
  let pth =
    log_lemma "SPEC.pth" (fun () ->
    let th1 = EQ_MP(AP_THM FORALL_DEF `P:A->bool`) (ASSUME `(!)(P:A->bool)`) in
    let th2 = AP_THM (CONV_RULE BETA_CONV th1) `x:A` in
    let th3 = CONV_RULE (RAND_CONV BETA_CONV) th2 in
    DISCH_ALL (EQT_ELIM th3)) in
  fun tm th ->
    try let abs = rand(concl th) in
        CONV_RULE BETA_CONV
         (MP (PINST [snd(dest_var(bndvar abs)),aty] [abs,P; tm,x] pth) th)
    with Failure _ -> failwith "SPEC";;

let SPEC = log_function2 "SPEC" log_term log_thm log_thm SPEC;;

let SPECL tms th =
  try rev_itlist SPEC tms th
  with Failure _ -> failwith "SPECL";;

let SPECL = log_function2 "SPECL" (log_list log_term) log_thm log_thm SPECL;;

let SPEC_VAR th =
  let bv = variant (thm_frees th) (bndvar(rand(concl th))) in
  bv,SPEC bv th;;

let SPEC_VAR =
    log_function "SPEC_VAR" log_thm (log_pair log_term log_thm) SPEC_VAR;;

let rec SPEC_ALL th =
  if is_forall(concl th) then SPEC_ALL(snd(SPEC_VAR th)) else th;;

let SPEC_ALL = log_function "SPEC_ALL" log_thm log_thm SPEC_ALL;;

let ISPEC t th =
  let x,_ = try dest_forall(concl th) with Failure _ ->
    failwith "ISPEC: input theorem not universally quantified" in
  let tyins = try type_match (snd(dest_var x)) (type_of t) [] with Failure _ ->
    failwith "ISPEC can't type-instantiate input theorem" in
  try SPEC t (INST_TYPE tyins th)
  with Failure _ -> failwith "ISPEC: type variable(s) free in assumptions";;

let ISPEC = log_function2 "ISPEC" log_term log_thm log_thm ISPEC;;

let ISPECL tms th =
  try if tms = [] then th else
      let avs = fst (chop_list (length tms) (fst(strip_forall(concl th)))) in
      let tyins = itlist2 type_match (map (snd o dest_var) avs)
                          (map type_of tms) [] in
      SPECL tms (INST_TYPE tyins th)
  with Failure _ -> failwith "ISPECL";;

let ISPECL = log_function2 "ISPECL" (log_list log_term) log_thm log_thm ISPECL;;

let GEN =
  let P = `P:A->bool`
  and pth =
    log_lemma "GEN.pth" (fun () ->
    let th1 = ASSUME `P = \x:A. T` in
    let th2 = AP_THM FORALL_DEF `P:A->bool` in
    EQ_MP (SYM(CONV_RULE(RAND_CONV BETA_CONV) th2)) th1) in
  fun x th ->
   PROVE_HYP (ABS x (EQT_INTRO th))
             (PINST [snd(dest_var x),aty] [mk_abs(x,concl th),P] pth);;

let GEN = log_function2 "GEN" log_term log_thm log_thm GEN;;

let GENL = itlist GEN;;

let GENL = log_function2 "GENL" (log_list log_term) log_thm log_thm GENL;;

let GEN_ALL th =
  let asl,c = dest_thm th in
  let vars = subtract (frees c) (freesl asl) in
  GENL vars th;;

let GEN_ALL = log_function "GEN_ALL" log_thm log_thm GEN_ALL;;

(* ------------------------------------------------------------------------- *)
(* Rules for ?                                                               *)
(* ------------------------------------------------------------------------- *)

let EXISTS =
  let P = `P:A->bool` and x = `x:A` in
  let pth =
    log_lemma "EXISTS.pth" (fun () ->
    let th1 = CONV_RULE (RAND_CONV BETA_CONV) (AP_THM EXISTS_DEF P) in
    let th2 = SPEC `x:A` (ASSUME `!x:A. P x ==> Q`) in
    let th3 = DISCH `!x:A. P x ==> Q` (MP th2 (ASSUME `(P:A->bool) x`)) in
    EQ_MP (SYM th1) (GEN `Q:bool` th3)) in
  fun (etm,stm) th ->
    try let qf,abs = dest_comb etm in
        let bth = BETA_CONV(mk_comb(abs,stm)) in
        let cth = PINST [type_of stm,aty] [abs,P; stm,x] pth in
        PROVE_HYP (EQ_MP (SYM bth) th) cth
    with Failure _ -> failwith "EXISTS";;

let EXISTS =
    log_function2 "EXISTS" (log_pair log_term log_term) log_thm log_thm EXISTS;;

let SIMPLE_EXISTS v th =
  EXISTS (mk_exists(v,concl th),v) th;;

let SIMPLE_EXISTS =
    log_function2 "SIMPLE_EXISTS" log_term log_thm log_thm SIMPLE_EXISTS;;

let CHOOSE =
  let P = `P:A->bool` and Q = `Q:bool` in
  let pth =
    log_lemma "CHOOSE.pth" (fun () ->
    let th1 = CONV_RULE (RAND_CONV BETA_CONV) (AP_THM EXISTS_DEF P) in
    let th2 = SPEC `Q:bool` (UNDISCH(fst(EQ_IMP_RULE th1))) in
    DISCH_ALL (DISCH `(?) (P:A->bool)` (UNDISCH th2))) in
  fun (v,th1) th2 ->
    try let abs = rand(concl th1) in
        let bv,bod = dest_abs abs in
        let cmb = mk_comb(abs,v) in
        let pat = vsubst[v,bv] bod in
        let th3 = CONV_RULE BETA_CONV (ASSUME cmb) in
        let th4 = GEN v (DISCH cmb (MP (DISCH pat th2) th3)) in
        let th5 = PINST [snd(dest_var v),aty] [abs,P; concl th2,Q] pth in
        MP (MP th5 th4) th1
    with Failure _ -> failwith "CHOOSE";;

let CHOOSE =
    log_function2 "CHOOSE" (log_pair log_term log_thm) log_thm log_thm CHOOSE;;

let SIMPLE_CHOOSE v th =
  CHOOSE(v,ASSUME (mk_exists(v,hd(hyp th)))) th;;

let SIMPLE_CHOOSE =
    log_function2 "SIMPLE_CHOOSE" log_term log_thm log_thm SIMPLE_CHOOSE;;

(* ------------------------------------------------------------------------- *)
(* Rules for \/                                                              *)
(* ------------------------------------------------------------------------- *)

let DISJ1 =
  let P = `P:bool` and Q = `Q:bool` in
  let pth =
    log_lemma "DISJ1.pth" (fun () ->
    let th1 = CONV_RULE (RAND_CONV BETA_CONV) (AP_THM OR_DEF `P:bool`) in
    let th2 = CONV_RULE (RAND_CONV BETA_CONV) (AP_THM th1 `Q:bool`) in
    let th3 = MP (ASSUME `P ==> t`) (ASSUME `P:bool`) in
    let th4 = GEN `t:bool` (DISCH `P ==> t` (DISCH `Q ==> t` th3)) in
    EQ_MP (SYM th2) th4) in
  fun th tm ->
    try PROVE_HYP th (INST [concl th,P; tm,Q] pth)
    with Failure _ -> failwith "DISJ1";;

let DISJ1 = log_function2 "DISJ1" log_thm log_term log_thm DISJ1;;

let DISJ2 =
  let P = `P:bool` and Q = `Q:bool` in
  let pth =
    log_lemma "DISJ2.pth" (fun () ->
    let th1 = CONV_RULE (RAND_CONV BETA_CONV) (AP_THM OR_DEF `P:bool`) in
    let th2 = CONV_RULE (RAND_CONV BETA_CONV) (AP_THM th1 `Q:bool`) in
    let th3 = MP (ASSUME `Q ==> t`) (ASSUME `Q:bool`) in
    let th4 = GEN `t:bool` (DISCH `P ==> t` (DISCH `Q ==> t` th3)) in
    EQ_MP (SYM th2) th4) in
  fun tm th ->
    try PROVE_HYP th (INST [tm,P; concl th,Q] pth)
    with Failure _ -> failwith "DISJ2";;

let DISJ2 = log_function2 "DISJ2" log_term log_thm log_thm DISJ2;;

let DISJ_CASES =
  let P = `P:bool` and Q = `Q:bool` and R = `R:bool` in
  let pth =
    log_lemma "DISJ_CASES.pth" (fun () ->
    let th1 = CONV_RULE (RAND_CONV BETA_CONV) (AP_THM OR_DEF `P:bool`) in
    let th2 = CONV_RULE (RAND_CONV BETA_CONV) (AP_THM th1 `Q:bool`) in
    let th3 = SPEC `R:bool` (EQ_MP th2 (ASSUME `P \/ Q`)) in
    UNDISCH (UNDISCH th3)) in
  fun th0 th1 th2 ->
    try let c1 = concl th1 and c2 = concl th2 in
        if not (aconv c1 c2) then failwith "DISJ_CASES" else
        let l,r = dest_disj (concl th0) in
        let th = INST [l,P; r,Q; c1,R] pth in
        PROVE_HYP (DISCH r th2) (PROVE_HYP (DISCH l th1) (PROVE_HYP th0 th))
    with Failure _ -> failwith "DISJ_CASES";;

let DISJ_CASES =
    log_function3 "DISJ_CASES" log_thm log_thm log_thm log_thm DISJ_CASES;;

let SIMPLE_DISJ_CASES th1 th2 =
  DISJ_CASES (ASSUME(mk_disj(hd(hyp th1),hd(hyp th2)))) th1 th2;;

let SIMPLE_DISJ_CASES =
    log_function2 "SIMPLE_DISJ_CASES" log_thm log_thm log_thm SIMPLE_DISJ_CASES;;

(* ------------------------------------------------------------------------- *)
(* Rules for negation and falsity.                                           *)
(* ------------------------------------------------------------------------- *)

let NOT_ELIM =
  let P = `P:bool` in
  let pth =
    log_lemma "NOT_ELIM.pth" (fun () ->
    CONV_RULE(RAND_CONV BETA_CONV) (AP_THM NOT_DEF P)) in
  fun th ->
    try EQ_MP (INST [rand(concl th),P] pth) th
    with Failure _ -> failwith "NOT_ELIM";;

let NOT_ELIM = log_function "NOT_ELIM" log_thm log_thm NOT_ELIM;;

let NOT_INTRO =
  let P = `P:bool` in
  let pth =
    log_lemma "NOT_INTRO.pth" (fun () ->
    SYM(CONV_RULE(RAND_CONV BETA_CONV) (AP_THM NOT_DEF P))) in
  fun th ->
    try EQ_MP (INST [rand(rator(concl th)),P] pth) th
    with Failure _ -> failwith "NOT_INTRO";;

let NOT_INTRO = log_function "NOT_INTRO" log_thm log_thm NOT_INTRO;;

let EQF_INTRO =
  let P = `P:bool` in
  let pth =
    log_lemma "EQF_INTRO.pth" (fun () ->
    let th1 = NOT_ELIM (ASSUME `~ P`)
    and th2 = DISCH `F` (SPEC P (EQ_MP F_DEF (ASSUME `F`))) in
    DISCH_ALL (IMP_ANTISYM_RULE th1 th2)) in
  fun th ->
    try MP (INST [rand(concl th),P] pth) th
    with Failure _ -> failwith "EQF_INTRO";;

let EQF_INTRO = log_function "EQF_INTRO" log_thm log_thm EQF_INTRO;;

let EQF_ELIM =
  let P = `P:bool` in
  let pth =
    log_lemma "EQF_ELIM.pth" (fun () ->
    let th1 = EQ_MP (ASSUME `P = F`) (ASSUME `P:bool`) in
    let th2 = DISCH P (SPEC `F` (EQ_MP F_DEF th1)) in
    DISCH_ALL (NOT_INTRO th2)) in
  fun th ->
    try MP (INST [rand(rator(concl th)),P] pth) th
    with Failure _ -> failwith "EQF_ELIM";;

let EQF_ELIM = log_function "EQF_ELIM" log_thm log_thm EQF_ELIM;;

let CONTR =
  let P = `P:bool` and f_tm = `F` in
  let pth =
    log_lemma "CONTR.pth" (fun () ->
    SPEC P (EQ_MP F_DEF (ASSUME `F`))) in
  fun tm th ->
    if concl th <> f_tm then failwith "CONTR"
    else PROVE_HYP th (INST [tm,P] pth);;

let CONTR = log_function2 "CONTR" log_term log_thm log_thm CONTR;;

(* ------------------------------------------------------------------------- *)
(* Rules for unique existence.                                               *)
(* ------------------------------------------------------------------------- *)

let EXISTENCE =
  let P = `P:A->bool` in
  let pth =
    log_lemma "EXISTENCE.pth" (fun () ->
    let th1 = CONV_RULE (RAND_CONV BETA_CONV) (AP_THM EXISTS_UNIQUE_DEF P) in
    let th2 = UNDISCH (fst(EQ_IMP_RULE th1)) in
    DISCH_ALL (CONJUNCT1 th2)) in
  fun th ->
    try let abs = rand(concl th) in
        let ty = snd(dest_var(bndvar abs)) in
        MP (PINST [ty,aty] [abs,P] pth) th
    with Failure _ -> failwith "EXISTENCE";;

let EXISTENCE = log_function "EXISTENCE" log_thm log_thm EXISTENCE;;

(* ------------------------------------------------------------------------- *)
(* Close out the logfile.                                                    *)
(* ------------------------------------------------------------------------- *)

logfile_end ();;
