(* ========================================================================= *)
(* A MINIMAL HIGHER ORDER LOGIC KERNEL                                       *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Thm =
sig

type thm

(* ------------------------------------------------------------------------- *)
(* Destructors                                                               *)
(* ------------------------------------------------------------------------- *)

datatype thm' =
    Thm of {id : int, axioms : SequentSet.set, sequent : Sequent.sequent}

val dest : thm -> thm'

(* ------------------------------------------------------------------------- *)
(*                                                                           *)
(* ----------  Axiom (A ?- t)                                                *)
(*   A |- t                                                                  *)
(*                                                                           *)
(* Note: theorems created by the Axiom rule are tagged, and tags are passed  *)
(* on by the primitive inference rules to all derived theorems.              *)
(* ------------------------------------------------------------------------- *)
val Axiom : Sequent.sequent -> thm

(* ------------------------------------------------------------------------- *)
(*         A |- t1 = t2                                                      *)
(* ----------------------------  Abs v                                       *)
(*   A |- (\v. t1) = (\v. t2)                                                *)
(*                                                                           *)
(* Side condition: the variable v must not be free in A.                     *)
(* ------------------------------------------------------------------------- *)
val Abs : Var.var -> thm -> thm

(* ------------------------------------------------------------------------- *)
(*                                                                           *)
(* ----------  Assume t                                                      *)
(*   t |- t                                                                  *)
(*                                                                           *)
(* Side condition: The term t must have boolean type.                        *)
(* ------------------------------------------------------------------------- *)
val Assume : Term.term -> thm

(* ------------------------------------------------------------------------- *)
(*                                                                           *)
(* -----------------------------  Beta_conv ((\v. t1) t2)                    *)
(*   |- (\v. t1) t2 = t1[t2/v]                                               *)
(* ------------------------------------------------------------------------- *)
val Beta_conv : Term.term -> thm

(* ------------------------------------------------------------------------- *)
(*           A |- t1    B |- t2                                              *)
(* --------------------------------------  Deduct_antisym                    *)
(*   (A - {t2}) u (B - {t1}) |- t1 = t2                                      *)
(* ------------------------------------------------------------------------- *)
val Deduct_antisym : thm -> thm -> thm

(* ------------------------------------------------------------------------- *)
(*   A |- t1 = t2    B |- t1'                                                *)
(* ----------------------------  Eq_mp                                       *)
(*         A u B |- t2                                                       *)
(*                                                                           *)
(* Side condition: the terms t1 and t1' must be alpha equivalent.            *)
(* ------------------------------------------------------------------------- *)
val Eq_mp : thm -> thm -> thm

(* ------------------------------------------------------------------------- *)
(*          A |- t                                                           *)
(* ------------------------  Subst theta                                     *)
(*   A[theta] |- t[theta]                                                    *)
(* ------------------------------------------------------------------------- *)
val Subst : TermSubst.subst -> thm -> thm

(* ------------------------------------------------------------------------- *)
(*   A |- f = g    B |- x = y                                                *)
(* ----------------------------  Mk_comb                                     *)
(*      A u B |- f x = g y                                                   *)
(*                                                                           *)
(* Side condition: the types of f and x must be compatible.                  *)
(* ------------------------------------------------------------------------- *)
val Mk_comb : thm -> thm -> thm

(* ------------------------------------------------------------------------- *)
(*                                                                           *)
(* ------------  Refl t                                                      *)
(*   |- t = t                                                                *)
(* ------------------------------------------------------------------------- *)
val Refl : Term.term -> thm

(* ------------------------------------------------------------------------- *)
(* Constant definition                                                       *)
(*                                                                           *)
(* ---------------  Define_const name t                                      *)
(*   |- name = t                                                             *)
(*                                                                           *)
(* where name is a new constant with the same type as the variable v.        *)
(*                                                                           *)
(* Side conditions: name is not an existing constant, t has no free          *)
(* variables, and all type variables in t also appear in the type of t.      *)
(* ------------------------------------------------------------------------- *)
val Define_const : Name.name -> Term.term -> thm

(* ------------------------------------------------------------------------- *)
(* Type operator definition                                                  *)
(*                                                                           *)
(*           |- P t                                                          *)
(* ------------------------------  Define_type name {abs,rep} ty_vars        *)
(*       |- abs (rep a) = a                                                  *)
(*   |- P r = (rep (abs r) = r)                                              *)
(*                                                                           *)
(* where if P has type 'a -> bool, then abs and rep are new constants with   *)
(* types 'a -> ty and ty -> 'a, respectively.                                *)
(*                                                                           *)
(* Side conditions: name is not an existing type operator, abs and rep do    *)
(* not have the same name as existing constants, and ty_vars lists all the   *)
(* type variables in P.                                                      *)
(* ------------------------------------------------------------------------- *)
val Define_type :
    Name.name -> {abs : Name.name, rep : Name.name} -> Name.name list -> thm ->
    thm * thm

end
