(* ========================================================================= *)
(* Trivial odds and ends.                                                    *)
(*                                                                           *)
(*       John Harrison, University of Cambridge Computer Laboratory          *)
(*                                                                           *)
(*            (c) Copyright, University of Cambridge 1998                    *)
(*              (c) Copyright, John Harrison 1998-2007                       *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* OpenTheory logging.                                                       *)
(* ------------------------------------------------------------------------- *)

logfile "trivia-comb-def";;

(* ------------------------------------------------------------------------- *)
(* Combinators. We don't bother with S and K, which seem of little use.      *)
(* ------------------------------------------------------------------------- *)

parse_as_infix ("o",(26,"right"));;

let o_DEF = new_definition
 `(o) (f:B->C) g = \x:A. f(g(x))`;;

let I_DEF = new_definition
 `I = \x:A. x`;;

(* ------------------------------------------------------------------------- *)
(* OpenTheory logging.                                                       *)
(* ------------------------------------------------------------------------- *)

logfile "trivia-comb-thm";;

let o_THM = log_lemma "o_THM" (fun () -> prove
 (`!f:B->C. !g:A->B. !x:A. (f o g) x = f(g(x))`,
  PURE_REWRITE_TAC [o_DEF] THEN
  CONV_TAC (DEPTH_CONV BETA_CONV) THEN
  REPEAT GEN_TAC THEN REFL_TAC));;

let o_ASSOC = log_lemma "o_ASSOC" (fun () -> prove
 (`!f:C->D. !g:B->C. !h:A->B. f o (g o h) = (f o g) o h`,
  REPEAT GEN_TAC THEN REWRITE_TAC [o_DEF] THEN
  CONV_TAC (REDEPTH_CONV BETA_CONV) THEN
  REFL_TAC));;

let I_THM = log_lemma "I_THM" (fun () -> prove
 (`!x:A. I x = x`,
  REWRITE_TAC [I_DEF]));;

let I_O_ID = log_lemma "I_O_ID" (fun () -> prove
 (`!f:A->B. (I o f = f) /\ (f o I = f)`,
  REPEAT STRIP_TAC THEN
  REWRITE_TAC[FUN_EQ_THM; o_DEF; I_THM]));;

(* ------------------------------------------------------------------------- *)
(* OpenTheory logging.                                                       *)
(* ------------------------------------------------------------------------- *)

logfile "trivia-one-def";;

(* ------------------------------------------------------------------------- *)
(* The theory "1" (a 1-element type).                                        *)
(* ------------------------------------------------------------------------- *)

let EXISTS_ONE_REP = log_lemma "EXISTS_ONE_REP" (fun () -> prove
 (`?b:bool. b`,
  EXISTS_TAC `T` THEN
  BETA_TAC THEN ACCEPT_TAC TRUTH));;

let one_tydef =
  new_type_definition "1" ("one_ABS","one_REP") EXISTS_ONE_REP;;

let one_DEF = new_definition
 `one = @x:1. T`;;

(* ------------------------------------------------------------------------- *)
(* OpenTheory logging.                                                       *)
(* ------------------------------------------------------------------------- *)

logfile "trivia-one-thm";;

let one = log_lemma "one" (fun () -> prove
 (`!v:1. v = one`,
  MP_TAC(GEN_ALL (SPEC `one_REP a` (CONJUNCT2 one_tydef))) THEN
  REWRITE_TAC[CONJUNCT1 one_tydef] THEN DISCH_TAC THEN
  ONCE_REWRITE_TAC[GSYM (CONJUNCT1 one_tydef)] THEN
  ASM_REWRITE_TAC[]));;

let one_axiom = log_lemma "one_axiom" (fun () -> prove
 (`!f g. f = (g:A->1)`,
  REPEAT GEN_TAC THEN ONCE_REWRITE_TAC[FUN_EQ_THM] THEN
  GEN_TAC THEN ONCE_REWRITE_TAC[one] THEN REFL_TAC));;

let one_INDUCT = log_lemma "one_INDUCT" (fun () -> prove
 (`!P. P one ==> !x. P x`,
  ONCE_REWRITE_TAC[one] THEN REWRITE_TAC[]));;

let one_RECURSION = log_lemma "one_RECURSION" (fun () -> prove
 (`!e:A. ?fn. fn one = e`,
  GEN_TAC THEN EXISTS_TAC `\x:1. e:A` THEN BETA_TAC THEN REFL_TAC));;

let one_Axiom = log_lemma "one_Axiom" (fun () -> prove
 (`!e:A. ?!fn. fn one = e`,
  GEN_TAC THEN REWRITE_TAC[EXISTS_UNIQUE_THM; one_RECURSION] THEN
  REPEAT STRIP_TAC THEN ONCE_REWRITE_TAC[FUN_EQ_THM] THEN
  ONCE_REWRITE_TAC [one] THEN ASM_REWRITE_TAC[]));;

(* ------------------------------------------------------------------------- *)
(* Add the type "1" to the inductive type store.                             *)
(* ------------------------------------------------------------------------- *)

inductive_type_store := 
  ("1",(1,one_INDUCT,one_RECURSION))::(!inductive_type_store);;

(* ------------------------------------------------------------------------- *)
(* Close out the logfile.                                                    *)
(* ------------------------------------------------------------------------- *)

logfile_end ();;
