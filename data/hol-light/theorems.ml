(* ========================================================================= *)
(* Additional theorems, mainly about quantifiers.                            *)
(*                                                                           *)
(*       John Harrison, University of Cambridge Computer Laboratory          *)
(*                                                                           *)
(*            (c) Copyright, University of Cambridge 1998                    *)
(*              (c) Copyright, John Harrison 1998-2007                       *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* OpenTheory logging.                                                       *)
(* ------------------------------------------------------------------------- *)

logfile "theorems-thm";;

(* ------------------------------------------------------------------------- *)
(* More stuff about equality.                                                *)
(* ------------------------------------------------------------------------- *)

let EQ_REFL = log_lemma "EQ_REFL" (fun () -> prove
 (`!x:A. x = x`,
  GEN_TAC THEN REFL_TAC));;

let EQ_REFL_T = log_lemma "EQ_REFL_T" (fun () -> prove
 (`!x:A. (x = x) <=> T`,
  GEN_TAC THEN MATCH_ACCEPT_TAC(EQT_INTRO(SPEC_ALL EQ_REFL))));;

let EQ_SYM = log_lemma "EQ_SYM" (fun () -> prove
 (`!(x:A) y. (x = y) ==> (y = x)`,
  REPEAT GEN_TAC THEN DISCH_THEN(ACCEPT_TAC o SYM)));;

let EQ_SYM_EQ = log_lemma "EQ_SYM_EQ" (fun () -> prove
 (`!(x:A) y. (x = y) <=> (y = x)`,
  REPEAT GEN_TAC THEN EQ_TAC THEN MATCH_ACCEPT_TAC EQ_SYM));;

let EQ_TRANS = log_lemma "EQ_TRANS" (fun () -> prove
 (`!(x:A) y z. (x = y) /\ (y = z) ==> (x = z)`,
  REPEAT STRIP_TAC THEN PURE_ASM_REWRITE_TAC[] THEN REFL_TAC));;

let REFL_CLAUSE = log_lemma "REFL_CLAUSE" (fun () -> prove
 (`!x:A. (x = x) = T`,
  GEN_TAC THEN ACCEPT_TAC(EQT_INTRO(SPEC `x:A` EQ_REFL))));;

(* ------------------------------------------------------------------------- *)
(* The following is a common special case of ordered rewriting.              *)
(* ------------------------------------------------------------------------- *)

let AC acsuite = EQT_ELIM o PURE_REWRITE_CONV[acsuite; EQ_REFL_T];;

let AC = log_function2 "AC" log_thm log_term log_thm AC;;

(* ------------------------------------------------------------------------- *)
(* A couple of theorems about beta reduction.                                *)
(* ------------------------------------------------------------------------- *)

let BETA_THM = log_lemma "BETA_THM" (fun () -> prove
 (`!(f:A->B) y. (\x. (f:A->B) x) y = f y`,
  REPEAT GEN_TAC THEN BETA_TAC THEN REFL_TAC));;

let ABS_SIMP = log_lemma "ABS_SIMP" (fun () -> prove
 (`!(t1:A) (t2:B). (\x. t1) t2 = t1`,
  REPEAT GEN_TAC THEN REWRITE_TAC[BETA_THM; REFL_CLAUSE]));;

(* ------------------------------------------------------------------------- *)
(* A few "big name" intuitionistic tautologies.                              *)
(* ------------------------------------------------------------------------- *)

let CONJ_ASSOC = log_lemma "CONJ_ASSOC" (fun () -> prove
 (`!t1 t2 t3. t1 /\ t2 /\ t3 <=> (t1 /\ t2) /\ t3`,
  ITAUT_TAC));;

let CONJ_SYM = log_lemma "CONJ_SYM" (fun () -> prove
 (`!t1 t2. t1 /\ t2 <=> t2 /\ t1`,
  ITAUT_TAC));;

let CONJ_ACI = log_lemma "CONJ_ACI" (fun () -> prove
 (`(p /\ q <=> q /\ p) /\
   ((p /\ q) /\ r <=> p /\ (q /\ r)) /\
   (p /\ (q /\ r) <=> q /\ (p /\ r)) /\
   (p /\ p <=> p) /\
   (p /\ (p /\ q) <=> p /\ q)`,
  ITAUT_TAC));;

let DISJ_ASSOC = log_lemma "DISJ_ASSOC" (fun () -> prove
 (`!t1 t2 t3. t1 \/ t2 \/ t3 <=> (t1 \/ t2) \/ t3`,
  ITAUT_TAC));;

let DISJ_SYM = log_lemma "DISJ_SYM" (fun () -> prove
 (`!t1 t2. t1 \/ t2 <=> t2 \/ t1`,
  ITAUT_TAC));;

let DISJ_ACI = log_lemma "DISJ_ACI" (fun () -> prove
 (`(p \/ q <=> q \/ p) /\
   ((p \/ q) \/ r <=> p \/ (q \/ r)) /\
   (p \/ (q \/ r) <=> q \/ (p \/ r)) /\
   (p \/ p <=> p) /\
   (p \/ (p \/ q) <=> p \/ q)`,
  ITAUT_TAC));;

let IMP_CONJ = log_lemma "IMP_CONJ" (fun () -> prove
 (`p /\ q ==> r <=> p ==> q ==> r`,
  ITAUT_TAC));;

let IMP_IMP = log_lemma "IMP_IMP" (fun () -> GSYM IMP_CONJ);;

let IMP_CONJ_ALT = log_lemma "IMP_CONJ_ALT" (fun () -> prove
 (`p /\ q ==> r <=> q ==> p ==> r`,
  ITAUT_TAC));;

(* ------------------------------------------------------------------------- *)
(* A couple of "distribution" tautologies are useful.                        *)
(* ------------------------------------------------------------------------- *)

let LEFT_OR_DISTRIB = log_lemma "LEFT_OR_DISTRIB" (fun () -> prove
 (`!p q r. p /\ (q \/ r) <=> p /\ q \/ p /\ r`,
  ITAUT_TAC));;

let RIGHT_OR_DISTRIB = log_lemma "RIGHT_OR_DISTRIB" (fun () -> prove
 (`!p q r. (p \/ q) /\ r <=> p /\ r \/ q /\ r`,
  ITAUT_TAC));;

(* ------------------------------------------------------------------------- *)
(* Degenerate cases of quantifiers.                                          *)
(* ------------------------------------------------------------------------- *)

let FORALL_SIMP = log_lemma "FORALL_SIMP" (fun () -> prove
 (`!t. (!x:A. t) = t`,
  ITAUT_TAC));;

let EXISTS_SIMP = log_lemma "EXISTS_SIMP" (fun () -> prove
 (`!t. (?x:A. t) = t`,
  ITAUT_TAC));;

(* ------------------------------------------------------------------------- *)
(* I also use this a lot (as a prelude to congruence reasoning).             *)
(* ------------------------------------------------------------------------- *)

let EQ_IMP = log_lemma "EQ_IMP" (fun () -> ITAUT `(a <=> b) ==> a ==> b`);;

(* ------------------------------------------------------------------------- *)
(* Start building up the basic rewrites; we add a few more later.            *)
(* ------------------------------------------------------------------------- *)

let EQ_CLAUSES = log_lemma "EQ_CLAUSES" (fun () -> prove
 (`!t. ((T <=> t) <=> t) /\ ((t <=> T) <=> t) /\
       ((F <=> t) <=> ~t) /\ ((t <=> F) <=> ~t)`,
  ITAUT_TAC));;

let NOT_CLAUSES_WEAK = log_lemma "NOT_CLAUSES_WEAK" (fun () -> prove
 (`(~T <=> F) /\ (~F <=> T)`,
  ITAUT_TAC));;

let AND_CLAUSES = log_lemma "AND_CLAUSES" (fun () -> prove
 (`!t. (T /\ t <=> t) /\ (t /\ T <=> t) /\ (F /\ t <=> F) /\
       (t /\ F <=> F) /\ (t /\ t <=> t)`,
  ITAUT_TAC));;

let OR_CLAUSES = log_lemma "OR_CLAUSES" (fun () -> prove
 (`!t. (T \/ t <=> T) /\ (t \/ T <=> T) /\ (F \/ t <=> t) /\
       (t \/ F <=> t) /\ (t \/ t <=> t)`,
  ITAUT_TAC));;

let IMP_CLAUSES = log_lemma "IMP_CLAUSES" (fun () -> prove
 (`!t. (T ==> t <=> t) /\ (t ==> T <=> T) /\ (F ==> t <=> T) /\
       (t ==> t <=> T) /\ (t ==> F <=> ~t)`,
  ITAUT_TAC));;

extend_basic_rewrites
  [REFL_CLAUSE;
   EQ_CLAUSES;
   NOT_CLAUSES_WEAK;
   AND_CLAUSES;
   OR_CLAUSES;
   IMP_CLAUSES;
   FORALL_SIMP;
   EXISTS_SIMP;
   BETA_THM;
   let IMP_EQ_CLAUSE = log_lemma "IMP_EQ_CLAUSE" (fun () -> prove
    (`((x = x) ==> p) <=> p`,
     REWRITE_TAC[EQT_INTRO(SPEC_ALL EQ_REFL); IMP_CLAUSES])) in
   IMP_EQ_CLAUSE];;

extend_basic_congs
  [log_lemma "anon basic conj" (fun () ->
   ITAUT `(p <=> p') ==> (p' ==> (q <=> q')) ==> (p ==> q <=> p' ==> q')`)];;

(* ------------------------------------------------------------------------- *)
(* Rewrite rule for unique existence.                                        *)
(* ------------------------------------------------------------------------- *)

let EXISTS_UNIQUE_THM = log_lemma "EXISTS_UNIQUE_THM" (fun () -> prove
 (`!P. (?!x:A. P x) <=> (?x. P x) /\ (!x x'. P x /\ P x' ==> (x = x'))`,
  GEN_TAC THEN REWRITE_TAC[EXISTS_UNIQUE_DEF]));;

(* ------------------------------------------------------------------------- *)
(* Trivial instances of existence.                                           *)
(* ------------------------------------------------------------------------- *)

let EXISTS_REFL = log_lemma "EXISTS_REFL" (fun () -> prove
 (`!a:A. ?x. x = a`,
  GEN_TAC THEN EXISTS_TAC `a:A` THEN REFL_TAC));;

let EXISTS_UNIQUE_REFL = log_lemma "EXISTS_UNIQUE_REFL" (fun () -> prove
 (`!a:A. ?!x. x = a`,
  GEN_TAC THEN REWRITE_TAC[EXISTS_UNIQUE_THM] THEN
  REPEAT(EQ_TAC ORELSE STRIP_TAC) THENL
   [EXISTS_TAC `a:A`; ASM_REWRITE_TAC[]] THEN
  REFL_TAC));;

(* ------------------------------------------------------------------------- *)
(* Unwinding.                                                                *)
(* ------------------------------------------------------------------------- *)

let UNWIND_THM1 = log_lemma "UNWIND_THM1" (fun () -> prove
 (`!P (a:A). (?x. (a = x) /\ P x) <=> P a`,
  REPEAT GEN_TAC THEN EQ_TAC THENL
   [DISCH_THEN(CHOOSE_THEN (CONJUNCTS_THEN2 SUBST1_TAC ACCEPT_TAC));
    DISCH_TAC THEN EXISTS_TAC `a:A` THEN
    CONJ_TAC THEN TRY(FIRST_ASSUM MATCH_ACCEPT_TAC) THEN
    REFL_TAC]));;

let UNWIND_THM2 = log_lemma "UNWIND_THM2" (fun () -> prove
 (`!P (a:A). (?x. (x = a) /\ P x) <=> P a`,
  REPEAT GEN_TAC THEN CONV_TAC(LAND_CONV(ONCE_DEPTH_CONV SYM_CONV)) THEN
  MATCH_ACCEPT_TAC UNWIND_THM1));;

(* ------------------------------------------------------------------------- *)
(* Permuting quantifiers.                                                    *)
(* ------------------------------------------------------------------------- *)

let SWAP_FORALL_THM = log_lemma "SWAP_FORALL_THM" (fun () -> prove
 (`!P:A->B->bool. (!x y. P x y) <=> (!y x. P x y)`,
  ITAUT_TAC));;

let SWAP_EXISTS_THM = log_lemma "SWAP_EXISTS_THM" (fun () -> prove
 (`!P:A->B->bool. (?x y. P x y) <=> (?y x. P x y)`,
  ITAUT_TAC));;

(* ------------------------------------------------------------------------- *)
(* Universal quantifier and conjunction.                                     *)
(* ------------------------------------------------------------------------- *)

let FORALL_AND_THM = log_lemma "FORALL_AND_THM" (fun () -> prove
 (`!P Q. (!x:A. P x /\ Q x) <=> (!x. P x) /\ (!x. Q x)`,
  ITAUT_TAC));;

let AND_FORALL_THM = log_lemma "AND_FORALL_THM" (fun () -> prove
 (`!P Q. (!x. P x) /\ (!x. Q x) <=> (!x:A. P x /\ Q x)`,
  ITAUT_TAC));;

let LEFT_AND_FORALL_THM = log_lemma "LEFT_AND_FORALL_THM" (fun () -> prove
 (`!P Q. (!x:A. P x) /\ Q <=> (!x:A. P x /\ Q)`,
  ITAUT_TAC));;

let RIGHT_AND_FORALL_THM = log_lemma "RIGHT_AND_FORALL_THM" (fun () -> prove
 (`!P Q. P /\ (!x:A. Q x) <=> (!x. P /\ Q x)`,
  ITAUT_TAC));;

(* ------------------------------------------------------------------------- *)
(* Existential quantifier and disjunction.                                   *)
(* ------------------------------------------------------------------------- *)

let EXISTS_OR_THM = log_lemma "EXISTS_OR_THM" (fun () -> prove
 (`!P Q. (?x:A. P x \/ Q x) <=> (?x. P x) \/ (?x. Q x)`,
  ITAUT_TAC));;

let OR_EXISTS_THM = log_lemma "OR_EXISTS_THM" (fun () -> prove
 (`!P Q. (?x. P x) \/ (?x. Q x) <=> (?x:A. P x \/ Q x)`,
  ITAUT_TAC));;

let LEFT_OR_EXISTS_THM = log_lemma "LEFT_OR_EXISTS_THM" (fun () -> prove
 (`!P Q. (?x. P x) \/ Q <=> (?x:A. P x \/ Q)`,
  ITAUT_TAC));;

let RIGHT_OR_EXISTS_THM = log_lemma "RIGHT_OR_EXISTS_THM" (fun () -> prove
 (`!P Q. P \/ (?x. Q x) <=> (?x:A. P \/ Q x)`,
  ITAUT_TAC));;

(* ------------------------------------------------------------------------- *)
(* Existential quantifier and conjunction.                                   *)
(* ------------------------------------------------------------------------- *)

let LEFT_EXISTS_AND_THM = log_lemma "LEFT_EXISTS_AND_THM" (fun () -> prove
 (`!P Q. (?x:A. P x /\ Q) <=> (?x:A. P x) /\ Q`,
  ITAUT_TAC));;

let RIGHT_EXISTS_AND_THM = log_lemma "RIGHT_EXISTS_AND_THM" (fun () -> prove
 (`!P Q. (?x:A. P /\ Q x) <=> P /\ (?x:A. Q x)`,
  ITAUT_TAC));;

let TRIV_EXISTS_AND_THM = log_lemma "TRIV_EXISTS_AND_THM" (fun () -> prove
 (`!P Q. (?x:A. P /\ Q) <=> (?x:A. P) /\ (?x:A. Q)`,
  ITAUT_TAC));;

let LEFT_AND_EXISTS_THM = log_lemma "LEFT_AND_EXISTS_THM" (fun () -> prove
 (`!P Q. (?x:A. P x) /\ Q <=> (?x:A. P x /\ Q)`,
  ITAUT_TAC));;

let RIGHT_AND_EXISTS_THM = log_lemma "RIGHT_AND_EXISTS_THM" (fun () -> prove
 (`!P Q. P /\ (?x:A. Q x) <=> (?x:A. P /\ Q x)`,
  ITAUT_TAC));;

let TRIV_AND_EXISTS_THM = log_lemma "TRIV_AND_EXISTS_THM" (fun () -> prove
 (`!P Q. (?x:A. P) /\ (?x:A. Q) <=> (?x:A. P /\ Q)`,
  ITAUT_TAC));;

(* ------------------------------------------------------------------------- *)
(* Only trivial instances of universal quantifier and disjunction.           *)
(* ------------------------------------------------------------------------- *)

let TRIV_FORALL_OR_THM = log_lemma "TRIV_FORALL_OR_THM" (fun () -> prove
 (`!P Q. (!x:A. P \/ Q) <=> (!x:A. P) \/ (!x:A. Q)`,
  ITAUT_TAC));;

let TRIV_OR_FORALL_THM = log_lemma "TRIV_OR_FORALL_THM" (fun () -> prove
 (`!P Q. (!x:A. P) \/ (!x:A. Q) <=> (!x:A. P \/ Q)`,
  ITAUT_TAC));;

(* ------------------------------------------------------------------------- *)
(* Implication and quantifiers.                                              *)
(* ------------------------------------------------------------------------- *)

let RIGHT_IMP_FORALL_THM = log_lemma "RIGHT_IMP_FORALL_THM" (fun () -> prove
 (`!P Q. (P ==> !x:A. Q x) <=> (!x. P ==> Q x)`,
  ITAUT_TAC));;

let RIGHT_FORALL_IMP_THM = log_lemma "RIGHT_FORALL_IMP_THM" (fun () -> prove
 (`!P Q. (!x. P ==> Q x) <=> (P ==> !x:A. Q x)`,
  ITAUT_TAC));;

let LEFT_IMP_EXISTS_THM = log_lemma "LEFT_IMP_EXISTS_THM" (fun () -> prove
 (`!P Q. ((?x:A. P x) ==> Q) <=> (!x. P x ==> Q)`,
  ITAUT_TAC));;

let LEFT_FORALL_IMP_THM = log_lemma "LEFT_FORALL_IMP_THM" (fun () -> prove
 (`!P Q. (!x. P x ==> Q) <=> ((?x:A. P x) ==> Q)`,
  ITAUT_TAC));;

let TRIV_FORALL_IMP_THM = log_lemma "TRIV_FORALL_IMP_THM" (fun () -> prove
 (`!P Q. (!x:A. P ==> Q) <=> ((?x:A. P) ==> (!x:A. Q))`,
  ITAUT_TAC));;

let TRIV_EXISTS_IMP_THM = log_lemma "TRIV_EXISTS_IMP_THM" (fun () -> prove
 (`!P Q. (?x:A. P ==> Q) <=> ((!x:A. P) ==> (?x:A. Q))`,
  ITAUT_TAC));;

(* ------------------------------------------------------------------------- *)
(* Alternative versions of unique existence.                                 *)
(* ------------------------------------------------------------------------- *)

let EXISTS_UNIQUE_ALT = log_lemma "EXISTS_UNIQUE_ALT" (fun () -> prove
 (`!P:A->bool. (?!x. P x) <=> (?x. !y. P y <=> (x = y))`,
  GEN_TAC THEN REWRITE_TAC[EXISTS_UNIQUE_THM] THEN EQ_TAC THENL
   [DISCH_THEN(CONJUNCTS_THEN2 (X_CHOOSE_TAC `x:A`) ASSUME_TAC) THEN
    EXISTS_TAC `x:A` THEN GEN_TAC THEN EQ_TAC THENL
     [DISCH_TAC THEN FIRST_ASSUM MATCH_MP_TAC THEN ASM_REWRITE_TAC[];
      DISCH_THEN(SUBST1_TAC o SYM) THEN FIRST_ASSUM MATCH_ACCEPT_TAC];
    DISCH_THEN(X_CHOOSE_TAC `x:A`) THEN
    ASM_REWRITE_TAC[GSYM EXISTS_REFL] THEN REPEAT GEN_TAC THEN
    DISCH_THEN(CONJUNCTS_THEN (SUBST1_TAC o SYM)) THEN REFL_TAC]));;

let EXISTS_UNIQUE = log_lemma "EXISTS_UNIQUE" (fun () -> prove
 (`!P:A->bool. (?!x. P x) <=> (?x. P x /\ !y. P y ==> (y = x))`,
  GEN_TAC THEN REWRITE_TAC[EXISTS_UNIQUE_ALT] THEN
  AP_TERM_TAC THEN ABS_TAC THEN
  GEN_REWRITE_TAC (LAND_CONV o BINDER_CONV)
   [ITAUT `(a <=> b) <=> (a ==> b) /\ (b ==> a)`] THEN
  GEN_REWRITE_TAC (LAND_CONV o ONCE_DEPTH_CONV) [EQ_SYM_EQ] THEN
  REWRITE_TAC[FORALL_AND_THM] THEN SIMP_TAC[] THEN
  REWRITE_TAC[LEFT_FORALL_IMP_THM; EXISTS_REFL] THEN
  REWRITE_TAC[CONJ_ACI]));;

(* ------------------------------------------------------------------------- *)
(* Close out the logfile.                                                    *)
(* ------------------------------------------------------------------------- *)

logfile_end ();;
