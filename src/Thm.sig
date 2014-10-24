(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEOREMS                                               *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Thm =
sig

(* ------------------------------------------------------------------------- *)
(* The abstract type of theorem.                                             *)
(* ------------------------------------------------------------------------- *)

type thm

(* ------------------------------------------------------------------------- *)
(* Theorem destructors.                                                      *)
(* ------------------------------------------------------------------------- *)

datatype thm' =
    Thm of
      {axioms : SequentSet.set,
       sequent : Sequent.sequent}

val dest : thm -> thm'

val axioms : thm -> SequentSet.set

val sequent : thm -> Sequent.sequent

val hyp : thm -> TermAlphaSet.set

val concl : thm -> Term.term

(* ------------------------------------------------------------------------- *)
(* A total order on theorems modulo alpha equivalence.                       *)
(* ------------------------------------------------------------------------- *)

val compare : thm * thm -> order

val equal : thm -> thm -> bool

val dealphaCompare : thm * thm -> order

val dealphaEqual : thm -> thm -> bool

(* ------------------------------------------------------------------------- *)
(* Primitive rules of inference.                                             *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(*                                                                           *)
(* ----------  axiom (A ?- t)                                                *)
(*   A |- t                                                                  *)
(*                                                                           *)
(* Note: theorems created by the axiom rule are tagged, and tags are passed  *)
(* on by the primitive inference rules to all derived theorems.              *)
(* ------------------------------------------------------------------------- *)

val axiom : Sequent.sequent -> thm

(* ------------------------------------------------------------------------- *)
(*        A |- t = u                                                         *)
(* --------------------------  abs v                                         *)
(*   A |- (\v. t) = (\v. u)                                                  *)
(*                                                                           *)
(* Side condition: the variable v must not be free in A.                     *)
(* ------------------------------------------------------------------------- *)

val abs : Var.var -> thm -> thm

(* ------------------------------------------------------------------------- *)
(*   A |- f = g    B |- x = y                                                *)
(* ----------------------------  app                                         *)
(*      A u B |- f x = g y                                                   *)
(*                                                                           *)
(* Side condition: the types of f and x must be compatible.                  *)
(* ------------------------------------------------------------------------- *)

val app : thm -> thm -> thm

(* ------------------------------------------------------------------------- *)
(*                                                                           *)
(* ----------  assume t                                                      *)
(*   t |- t                                                                  *)
(*                                                                           *)
(* Side condition: The term t must have boolean type.                        *)
(* ------------------------------------------------------------------------- *)

val assume : Term.term -> thm

(* ------------------------------------------------------------------------- *)
(*                                                                           *)
(* -------------------------  betaConv ((\v. t) u)                           *)
(*   |- (\v. t) u = t[u/v]                                                   *)
(* ------------------------------------------------------------------------- *)

val betaConv : Term.term -> thm

(* ------------------------------------------------------------------------- *)
(*         A |- t    B |- u                                                  *)
(* ----------------------------------  deductAntisym                         *)
(*   (A - {u}) u (B - {t}) |- t = u                                          *)
(* ------------------------------------------------------------------------- *)

val deductAntisym : thm -> thm -> thm

(* ------------------------------------------------------------------------- *)
(*   A |- t = u    B |- t'                                                   *)
(* -------------------------  eqMp                                           *)
(*         A u B |- u                                                        *)
(*                                                                           *)
(* Side condition: the terms t and t' must be alpha equivalent.              *)
(* ------------------------------------------------------------------------- *)

val eqMp : thm -> thm -> thm

(* ------------------------------------------------------------------------- *)
(*                                                                           *)
(* ------------  refl t                                                      *)
(*   |- t = t                                                                *)
(* ------------------------------------------------------------------------- *)

val refl : Term.term -> thm

(* ------------------------------------------------------------------------- *)
(*          A |- t                                                           *)
(* ------------------------  Subst theta                                     *)
(*   A[theta] |- t[theta]                                                    *)
(* ------------------------------------------------------------------------- *)

val subst : TermSubst.subst -> thm -> thm

(* ------------------------------------------------------------------------- *)
(* Constant definition                                                       *)
(*                                                                           *)
(* ---------------  defineConst name t                                       *)
(*   |- name = t                                                             *)
(*                                                                           *)
(* where name is a new constant with the same type as the term t.            *)
(*                                                                           *)
(* Side conditions: t has no free variables, and all type variables in t     *)
(* also appear in the type of t.                                             *)
(* ------------------------------------------------------------------------- *)

val defineConst : Name.name -> Term.term -> Const.const * thm

(* ------------------------------------------------------------------------- *)
(* Type operator definition                                                  *)
(*                                                                           *)
(*             |- p t                                                        *)
(* ----------------------------------  defineTypeOp name {abs} {rep} tyVars  *)
(*   |- (\a. abs (rep a)) = (\a. a)                                          *)
(*   |- p = (\r. rep (abs r) = r)                                            *)
(*                                                                           *)
(* where if p has type 'a -> bool, then abs and rep are new constants with   *)
(* types 'a -> ty and ty -> 'a, respectively.                                *)
(*                                                                           *)
(* Side condition: tyVars lists all the type variables in p precisely once.  *)
(* ------------------------------------------------------------------------- *)

val defineTypeOp :
    Name.name -> {abs : Name.name} -> {rep : Name.name} -> Name.name list ->
    thm ->
    TypeOp.typeOp * {abs : Const.const} * {rep : Const.const} * thm * thm

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {sequentGrammar : Sequent.grammar}

val defaultGrammar : grammar

val ppWithGrammar : grammar -> Show.show -> thm Print.pp

val ppWithShow : Show.show -> thm Print.pp

val pp : thm Print.pp

val toString : thm -> string

end
