(* ========================================================================= *)
(* FORWARD INFERENCE RULES                                                   *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Rule =
sig

(* ------------------------------------------------------------------------- *)
(* Primitive rules, repeated from the logical kernel.                        *)
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
(* Primitive definition rules permitting alpha equivalent redefinitions.     *)
(* ------------------------------------------------------------------------- *)

type constDef = {tm : Term.term, def : Thm.thm}

type typeDef =
     {abs : Name.name, rep : Name.name, tyVars : Name.name list,
      nonEmptyTh : Thm.thm, absRepTh : Thm.thm, repAbsTh : Thm.thm}

val constDef : Name.name -> constDef option

val typeDef : Name.name -> typeDef option

val defineConst : Name.name -> Term.term -> Thm.thm

val defineType :
    Name.name -> {abs : Name.name, rep : Name.name} -> Name.name list ->
    Thm.thm -> Thm.thm * Thm.thm

(* ------------------------------------------------------------------------- *)
(* Derived rules.                                                            *)
(* ------------------------------------------------------------------------- *)

(* Alpha conversion *)

val alpha : Sequent.sequent -> Thm.thm -> Thm.thm

(* Transitivity of equality *)

val trans : Thm.thm -> Thm.thm -> Thm.thm

(* Constant definition by supplying the required theorem *)

val define : Term.term -> Thm.thm

end
