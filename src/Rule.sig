(* ========================================================================= *)
(* FORWARD INFERENCE RULES                                                   *)
(* Copyright (c) 2004 Joe Hurd, distributed under the MIT license            *)
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

end
