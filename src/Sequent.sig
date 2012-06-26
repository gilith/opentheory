(* ========================================================================= *)
(* HIGHER ORDER LOGIC SEQUENTS                                               *)
(* Copyright (c) 2004 Joe Hurd, distributed under the MIT license            *)
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

val isBool : sequent -> bool

val checkBool : sequent -> unit

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
(* Substitutions.                                                            *)
(* ------------------------------------------------------------------------- *)

val sharingSubst :
    sequent -> TermSubst.subst -> sequent option * TermSubst.subst

val subst : TermSubst.subst -> sequent -> sequent option

(* ------------------------------------------------------------------------- *)
(* Rewrites.                                                                 *)
(* ------------------------------------------------------------------------- *)

val sharingRewrite :
    sequent -> TermRewrite.rewrite -> sequent option * TermRewrite.rewrite

val rewrite : TermRewrite.rewrite -> sequent -> sequent option

(* ------------------------------------------------------------------------- *)
(* Searching for subterms.                                                   *)
(* ------------------------------------------------------------------------- *)

val sharingSearch :
    sequent -> TermSearch.search -> Term.term option * TermSearch.search

val search : TermSearch.search -> sequent -> Term.term option

(* ------------------------------------------------------------------------- *)
(* Axioms.                                                                   *)
(* ------------------------------------------------------------------------- *)

val axiomOfExtensionality : sequent

val axiomOfChoice : sequent

val axiomOfInfinity : sequent

val standardAxioms : sequent list

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {connective : Print.token,
       hypGrammar : Term.grammar,
       conclGrammar : Term.grammar,
       ppConnective : (sequent * Print.token) Print.pp,
       showHyp : bool}

val defaultGrammar : grammar

val ppWithGrammar : grammar -> Show.show -> sequent Print.pp

val ppWithShow : Show.show -> sequent Print.pp

val pp : sequent Print.pp

val toString : sequent -> string

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

val htmlGrammar : grammar

val ppHtml : Show.show -> sequent Print.pp

val toHtmlWithGrammar : grammar -> Show.show -> sequent -> Html.block

val toHtml : Show.show -> sequent -> Html.block

end
