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

type requireName = string

type name = string

datatype require =
    Require of
      {name : requireName,
       requires : requireName list,
       interpretation : Interpretation.interpretation,
       package : name}

type theory = requireName Theory.theory

datatype package =
    Package of
      {tags : tag list,
       requires : require list,
       theory : theory}

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val ppTag : tag Print.pp

val ppRequireName : requireName Print.pp

val ppName : name Print.pp

val ppRequire : require Print.pp

val ppTheory : theory Print.pp

val pp : package Print.pp

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parserTag : (char,tag) Parse.parser

val parserRequireName : (char,requireName) Parse.parser

val parserName : (char,name) Parse.parser

val parserRequire : (char,require) Parse.parser

val parserTheory : (char,theory) Parse.parser

val parser : (char,package) Parse.parser

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

val fromTextFile : {filename : string} -> package

val toTextFile : {package : package, filename : string} -> unit

end
