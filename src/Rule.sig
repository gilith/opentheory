(* ========================================================================= *)
(* FORWARD INFERENCE RULES                                                   *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Rule =
sig

(* ------------------------------------------------------------------------- *)
(* Applying equalities at subterms.                                          *)
(* ------------------------------------------------------------------------- *)

val rator : Thm.thm -> Term.term -> Thm.thm

val rand : Term.term -> Thm.thm -> Thm.thm

(* ------------------------------------------------------------------------- *)
(* Symmetry of equality.                                                     *)
(* ------------------------------------------------------------------------- *)

val sym : Thm.thm -> Thm.thm

(* ------------------------------------------------------------------------- *)
(* Transitivity of equality.                                                 *)
(* ------------------------------------------------------------------------- *)

val trans : Thm.thm -> Thm.thm -> Thm.thm

(* ------------------------------------------------------------------------- *)
(* Proving hypotheses.                                                       *)
(* ------------------------------------------------------------------------- *)

val proveHyp : Thm.thm -> Thm.thm -> Thm.thm

(* ------------------------------------------------------------------------- *)
(* Alpha conversion.                                                         *)
(* ------------------------------------------------------------------------- *)

val alpha : Sequent.sequent -> Thm.thm -> Thm.thm

val findAlpha : ThmSet.set -> Sequent.sequent -> Thm.thm option

(* ------------------------------------------------------------------------- *)
(* The new principle of constant definition.                                 *)
(* ------------------------------------------------------------------------- *)

val defineConstList :
    (Name.name * Var.var) list -> Thm.thm -> Const.const list * Thm.thm

(* ------------------------------------------------------------------------- *)
(* The legacy (a.k.a. HOL Light) version of defineTypeOp.                    *)
(* ------------------------------------------------------------------------- *)

val defineTypeOpLegacy :
    Name.name -> {abs : Name.name} -> {rep : Name.name} -> Name.name list ->
    Thm.thm ->
    TypeOp.typeOp * {abs : Const.const} * {rep : Const.const} *
    Thm.thm * Thm.thm

end
