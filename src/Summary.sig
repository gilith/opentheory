(* ========================================================================= *)
(* THEORY SUMMARIES                                                          *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
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
(* A type of theory contexts.                                                *)
(* ------------------------------------------------------------------------- *)

datatype context =
    NoContext
  | Context of
      {groundedInput : Symbol.symbol -> bool,
       satisfiedAssumption : Sequent.sequent -> bool}

(* ------------------------------------------------------------------------- *)
(* Check summary.                                                            *)
(* ------------------------------------------------------------------------- *)

val check : {checkTheorems : bool} -> context -> Show.show -> summary -> unit

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
       showTheoremAssumptions : bool}

val defaultGrammar : grammar

val ppWithGrammar : grammar -> context -> Show.show -> summary Print.pp

val ppWithContext : context -> Show.show -> summary Print.pp

val ppWithShow : Show.show -> summary Print.pp

val pp : summary Print.pp

val toTextFileWithGrammar :
    grammar ->
    {context : context,
     show : Show.show,
     summary : summary,
     filename : string} -> unit

val toTextFile :
    {context : context,
     show : Show.show,
     summary : summary,
     filename : string} -> unit

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

val htmlGrammar : grammar

val toHtmlWithGrammar :
    grammar -> context -> Show.show -> summary -> Html.block list

val toHtmlWithContext : context -> Show.show -> summary -> Html.block list

val toHtml : Show.show -> summary -> Html.block list

end
