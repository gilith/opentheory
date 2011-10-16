(* ========================================================================= *)
(* THEORY SUMMARIES                                                          *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Summary =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory summaries.                                               *)
(* ------------------------------------------------------------------------- *)

type summary

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype summary' =
    Summary' of
      {requires : Sequents.sequents,
       provides : Sequents.sequents}

val mk : summary' -> summary

val dest : summary -> summary'

val requires : summary -> Sequents.sequents

val provides : summary -> Sequents.sequents

val fromThms : Thms.thms -> summary

(* ------------------------------------------------------------------------- *)
(* Substitutions.                                                            *)
(* ------------------------------------------------------------------------- *)

val sharingSubst :
    summary -> TermSubst.subst -> summary option * TermSubst.subst

val subst : TermSubst.subst -> summary -> summary option

(* ------------------------------------------------------------------------- *)
(* Rewrites.                                                                 *)
(* ------------------------------------------------------------------------- *)

val sharingRewrite :
    summary -> TermRewrite.rewrite -> summary option * TermRewrite.rewrite

val rewrite : TermRewrite.rewrite -> summary -> summary option

(* ------------------------------------------------------------------------- *)
(* Check summary.                                                            *)
(* ------------------------------------------------------------------------- *)

val check : (Sequent.sequent -> bool) option -> Show.show -> summary -> unit

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {assumptionGrammar : Sequent.grammar,
       axiomGrammar : Sequent.grammar,
       theoremGrammar : Sequent.grammar,
       ppTypeOp : Show.show -> TypeOp.typeOp Print.pp,
       ppConst : Show.show -> Const.const Print.pp,
       unsatisfiedAssumptions : (Sequent.sequent -> bool) option,
       showTheoremAssumptions : bool}

val defaultGrammar : grammar

val ppWithGrammar : grammar -> Show.show -> summary Print.pp

val ppWithShow : Show.show -> summary Print.pp

val pp : summary Print.pp

val toTextFileWithGrammar :
    grammar ->
    {show : Show.show,
     summary : summary,
     filename : string} -> unit

val toTextFile :
    {show : Show.show,
     summary : summary,
     filename : string} -> unit

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

val htmlGrammar : grammar

val toHtmlWithGrammar : grammar -> Show.show -> summary -> Html.block list

val toHtml : Show.show -> summary -> Html.block list

end
