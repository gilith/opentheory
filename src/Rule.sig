(* ========================================================================= *)
(* FORWARD INFERENCE RULES                                                   *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Rule =
sig

(* ------------------------------------------------------------------------- *)
(* Transitivity of equality.                                                 *)
(* ------------------------------------------------------------------------- *)

val trans : Thm.thm -> Thm.thm -> Thm.thm

(* ------------------------------------------------------------------------- *)
(* Alpha conversion.                                                         *)
(* ------------------------------------------------------------------------- *)

val alpha : Sequent.sequent -> Thm.thm -> Thm.thm

val findAlpha : ThmSet.set -> Sequent.sequent -> Thm.thm option

(* ------------------------------------------------------------------------- *)
(* Alpha conversion ignoring definitions of type operators and constants.    *)
(* ------------------------------------------------------------------------- *)

val redefAlpha : Sequent.sequent -> Thm.thm -> Thm.thm

end
