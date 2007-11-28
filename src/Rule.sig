(* ========================================================================= *)
(* FORWARD INFERENCE RULES                                                   *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Rule =
sig

(* ------------------------------------------------------------------------- *)
(* Primitive rules, repeated from the logical kernel                         *)
(* ------------------------------------------------------------------------- *)

val Axiom : Sequent.sequent -> Thm.thm

val Abs : Var.var -> Thm.thm -> Thm.thm

val Assume : Term.term -> Thm.thm

val Beta_conv : Term.term -> Thm.thm

val Deduct_antisym : Thm.thm -> Thm.thm -> Thm.thm

val Eq_mp : Thm.thm -> Thm.thm -> Thm.thm

val Subst : TermSubst.subst -> Thm.thm -> Thm.thm

val Mk_comb : Thm.thm -> Thm.thm -> Thm.thm

val Refl : Term.term -> Thm.thm

(* ------------------------------------------------------------------------- *)
(* Primitive definition rules permitting alpha equivalent redefinitions      *)
(* ------------------------------------------------------------------------- *)

val Define_const : Name.name -> Term.term -> Thm.thm

val Define_type :
    Name.name -> {abs : Name.name, rep : Name.name} -> Name.name list ->
    Thm.thm -> Thm.thm * Thm.thm

(* ------------------------------------------------------------------------- *)
(* Derived rules                                                             *)
(* ------------------------------------------------------------------------- *)

val Alpha : Term.term list * Term.term -> Thm.thm -> Thm.thm

val Trans : Thm.thm -> Thm.thm -> Thm.thm

val Define : Term.term -> Thm.thm

end
