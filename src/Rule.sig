(* ========================================================================= *)
(* FORWARD INFERENCE RULES                                                   *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Rule =
sig

(* ------------------------------------------------------------------------- *)
(* Primitive rules, repeated from the logical kernel                         *)
(* ------------------------------------------------------------------------- *)

val axiom : Sequent.sequent -> Thm.thm

val abs : Var.var -> Thm.thm -> Thm.thm

val assume : Term.term -> Thm.thm

val betaConv : Term.term -> Thm.thm

val deductAntisym : Thm.thm -> Thm.thm -> Thm.thm

val eqMp : Thm.thm -> Thm.thm -> Thm.thm

val subst : TermSubst.subst -> Thm.thm -> Thm.thm

val comb : Thm.thm -> Thm.thm -> Thm.thm

val refl : Term.term -> Thm.thm

(* ------------------------------------------------------------------------- *)
(* Primitive definition rules permitting alpha equivalent redefinitions      *)
(* ------------------------------------------------------------------------- *)

val defineConst : Name.name -> Term.term -> Thm.thm

val defineType :
    Name.name -> {abs : Name.name, rep : Name.name} -> Name.name list ->
    Thm.thm -> Thm.thm * Thm.thm

(* ------------------------------------------------------------------------- *)
(* Derived rules                                                             *)
(* ------------------------------------------------------------------------- *)

val alpha : Term.term list * Term.term -> Thm.thm -> Thm.thm

val trans : Thm.thm -> Thm.thm -> Thm.thm

val define : Term.term -> Thm.thm

end
