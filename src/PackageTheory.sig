(* ========================================================================= *)
(* THEORIES OF HIGHER ORDER LOGIC USED IN PACKAGES                           *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageTheory =
sig

(* ------------------------------------------------------------------------- *)
(* Types of package theory syntax.                                           *)
(* ------------------------------------------------------------------------- *)

type name = PackageBase.base

datatype body =
    Package of Interpretation.interpretation * PackageName.name
  | Article of Interpretation.interpretation * {filename : string}
  | Union

datatype theory =
    Theory of
      {imports : name list,
       body : body}

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val imports : theory -> name list

val body : theory -> body

(* ------------------------------------------------------------------------- *)
(* Article dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val destArticleBody : body -> {filename : string} option

val destArticle : theory -> {filename : string} option

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val destPackageBody : body -> PackageName.name option

val destPackage : theory -> PackageName.name option

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppName : name Print.pp

val pp : theory Print.pp

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parserName : (char,name) Parse.parser

val parser : (char,theory) Parse.parser

end
