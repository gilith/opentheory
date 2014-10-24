(* ========================================================================= *)
(* FORWARD INFERENCE RULES                                                   *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Rule =
sig

(* ------------------------------------------------------------------------- *)
(* The legacy (a.k.a. HOL Light) version of defineTypeOp.                    *)
(* ------------------------------------------------------------------------- *)

val defineTypeOpLegacy :
    Name.name -> {abs : Name.name} -> {rep : Name.name} -> Name.name list ->
    Thm.thm ->
    TypeOp.typeOp * {abs : Const.const} * {rep : Const.const} *
    Thm.thm * Thm.thm

(* ------------------------------------------------------------------------- *)
(* Transitivity of equality.                                                 *)
(* ------------------------------------------------------------------------- *)

val trans : Thm.thm -> Thm.thm -> Thm.thm

(* ------------------------------------------------------------------------- *)
(* Alpha conversion.                                                         *)
(* ------------------------------------------------------------------------- *)

val alpha : Sequent.sequent -> Thm.thm -> Thm.thm

val findAlpha : ThmSet.set -> Sequent.sequent -> Thm.thm option

end
