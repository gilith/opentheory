(* ========================================================================= *)
(* PACKAGE NAMES                                                             *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageName =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory package names.                                           *)
(* ------------------------------------------------------------------------- *)

type name

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype name' =
    Name' of
      {base : PackageBase.base,
       version : PackageVersion.version};

val mk : name' -> name

val dest : name -> name'

val base : name -> PackageBase.base

val version : name -> PackageVersion.version

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : name * name -> order

val equal : name -> name -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : name Print.pp

val toString : name -> string

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parser : (char,name) Parse.parser

val fromString : string -> name

end
