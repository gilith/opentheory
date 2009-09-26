(* ========================================================================= *)
(* HIGHER ORDER LOGIC SEQUENTS                                               *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Sequent =
sig

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic sequents.                                    *)
(* ------------------------------------------------------------------------- *)

datatype sequent =
    Sequent of
      {hyp : TermAlphaSet.set,
       concl : Term.term}

val hyp : sequent -> TermAlphaSet.set

val concl : sequent -> Term.term

(* ------------------------------------------------------------------------- *)
(* Checking the hypotheses and conclusion are of type bool.                  *)
(* ------------------------------------------------------------------------- *)

val boolean : sequent -> bool

(* ------------------------------------------------------------------------- *)
(* A total order on sequents modulo alpha equivalence.                       *)
(* ------------------------------------------------------------------------- *)

val compare : sequent * sequent -> order

val equal : sequent -> sequent -> bool

(* ------------------------------------------------------------------------- *)
(* Type operators and constants.                                             *)
(* ------------------------------------------------------------------------- *)

val typeOps : sequent -> TypeOpSet.set

val consts : sequent -> ConstSet.set

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val showHyp : bool ref

val ppGen : {showHyp : bool, connective : string} -> sequent Print.pp

val pp : sequent Print.pp

val toString : sequent -> string

end
