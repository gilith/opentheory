(* ========================================================================= *)
(* PACKAGE NAMES                                                             *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageName =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory package names.                                           *)
(* ------------------------------------------------------------------------- *)

type name

(* ------------------------------------------------------------------------- *)
(* Concatenation.                                                            *)
(* ------------------------------------------------------------------------- *)

val append : name -> name -> name

val concat : name list -> name

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : name * name -> order

val equal : name -> name -> bool

(* ------------------------------------------------------------------------- *)
(* Generating fresh names.                                                   *)
(* ------------------------------------------------------------------------- *)

val variantName : {avoid : name -> bool} -> name -> name

(* ------------------------------------------------------------------------- *)
(* Prefix and suffix names.                                                  *)
(* ------------------------------------------------------------------------- *)

val destStrictPrefix : name -> name -> name option

val isStrictPrefix : name -> name -> bool

val isPrefix : name -> name -> bool

val destStrictSuffix : name -> name -> name option

val isStrictSuffix : name -> name -> bool

val isSuffix : name -> name -> bool

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

(* ------------------------------------------------------------------------- *)
(* Theory block names.                                                       *)
(* ------------------------------------------------------------------------- *)

val mainTheory : name

(* ------------------------------------------------------------------------- *)
(* Tag names.                                                                *)
(* ------------------------------------------------------------------------- *)

(* Package basics *)

val authorTag : name

val descriptionTag : name

val licenseTag : name

val nameTag : name

val versionTag : name

(* Extra package files *)

val extraSuffixTag : name

(* Shows *)

val showTag : name

(* ------------------------------------------------------------------------- *)
(* Directory checksums names.                                                *)
(* ------------------------------------------------------------------------- *)

val installedChecksums : name

(* ------------------------------------------------------------------------- *)
(* Repo names.                                                               *)
(* ------------------------------------------------------------------------- *)

val gilithRepo : name

(* ------------------------------------------------------------------------- *)
(* Haskell export names.                                                     *)
(* ------------------------------------------------------------------------- *)

val haskellExport : name

val newHaskellExport : name

val srcHaskellExport : name

val testHaskellExport : name

end
