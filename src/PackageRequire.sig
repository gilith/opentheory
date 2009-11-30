(* ========================================================================= *)
(* REQUIRED THEORY PACKAGES                                                  *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageRequire =
sig

(* ------------------------------------------------------------------------- *)
(* A type of required theory packages.                                       *)
(* ------------------------------------------------------------------------- *)

type name = string

datatype require =
    Require of
      {name : name,
       requires : name list,
       interpretation : Interpretation.interpretation,
       package : PackageName.name}

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val name : require -> name

val package : require -> PackageName.name

(* ------------------------------------------------------------------------- *)
(* Topological sort of requirements.                                         *)
(* ------------------------------------------------------------------------- *)

val sort : require list -> require list

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppName : name Print.pp

val pp : require Print.pp

val ppList : require list Print.pp

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parserName : (char,name) Parse.parser

val parser : (char,require) Parse.parser

val parserList : (char, require list) Parse.parser

end
