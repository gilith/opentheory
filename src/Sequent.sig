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

val dealphaCompare : sequent * sequent -> order

val dealphaEqual : sequent -> sequent -> bool

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

val addSharingTypeOps : sequent -> Term.sharingTypeOps -> Term.sharingTypeOps

val typeOps : sequent -> TypeOpSet.set

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val addSharingConsts : sequent -> Term.sharingConsts -> Term.sharingConsts

val consts : sequent -> ConstSet.set

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {connective : string,
       hypGrammar : Term.grammar,
       conclGrammar : Term.grammar,
       showHyp : bool}

val defaultGrammar : grammar

val ppWithGrammar : grammar -> Show.show -> sequent Print.pp

val ppWithShow : Show.show -> sequent Print.pp

val pp : sequent Print.pp

val toString : sequent -> string

val ppHtml : Show.show -> sequent Print.pp

val toHtml : Show.show -> sequent -> Html.block

end
