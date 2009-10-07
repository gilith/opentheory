(* ========================================================================= *)
(* HIGHER ORDER LOGIC THEORY PACKAGE SYNTAX                                  *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Package =
sig

(* ------------------------------------------------------------------------- *)
(* Types of theory package syntax.                                           *)
(* ------------------------------------------------------------------------- *)

datatype tag =
    Tag of
      {field : string,
       value : string}

datatype require =
    Require of
      {name : string,
       package : string,
       interpretation : Interpretation.interpretation,
       import : string list}

datatype theory =
    Local of theory * theory
  | Sequence of theory list
  | Article of {filename : string}
  | Interpret of Interpretation.interpretation * theory
  | Import of {require : string}

datatype package =
    Package of
      {tags : tag list,
       requires : require list,
       theory : theory}

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppTag : tag Print.pp

val ppRequire : require Print.pp

val ppTheory : theory Print.pp

val pp : package Print.pp

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parserTag : (char,tag) Parse.parser

val parserRequire : (char,require) Parse.parser

val parserTheory : (char,theory) Parse.parser

val parser : (char,package) Parse.parser

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

val fromTextFile : {filename : string} -> package

val toTextFile : {package : package, filename : string} -> unit

end
