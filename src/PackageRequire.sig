(* ========================================================================= *)
(* REQUIRED THEORY PACKAGES                                                  *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageRequire =
sig

(* ------------------------------------------------------------------------- *)
(* A type of required theory packages.                                       *)
(* ------------------------------------------------------------------------- *)

datatype require =
    Require of
      {name : PackageTheory.name,
       theory : PackageTheory.theory}

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val name : require -> PackageTheory.name

val theory : require -> PackageTheory.theory

val imports : require -> PackageTheory.name list

val body : require -> PackageTheory.body

(* ------------------------------------------------------------------------- *)
(* Article dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val article : require -> {filename : string} option

val articles : require list -> {filename : string} list

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val package : require -> PackageName.name option

val packages : require list -> PackageName.name list

(* ------------------------------------------------------------------------- *)
(* Topological sort of requirements.                                         *)
(* ------------------------------------------------------------------------- *)

val sort : require list -> require list

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : require Print.pp

val ppList : require list Print.pp

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parser : (char,require) Parse.parser

val parserList : (char, require list) Parse.parser

end
