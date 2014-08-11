(* ========================================================================= *)
(* PACKAGE NAMES                                                             *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
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

val strictPrefixes : name -> name list

val prefixes : name -> name list

val destStrictSuffix : name -> name -> name option

val isStrictSuffix : name -> name -> bool

val isSuffix : name -> name -> bool

val strictSuffixes : name -> name list

val suffixes : name -> name list

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

val checksumTag : name

(* Extra package files *)

val extraSuffixTag : name

(* Package requirements *)

val requiresTag : name

(* Shows *)

val showTag : name

(* ------------------------------------------------------------------------- *)
(* Repository list of installed packages and checksums.                      *)
(* ------------------------------------------------------------------------- *)

val installedChecksums : name

(* ------------------------------------------------------------------------- *)
(* Remote repository names.                                                  *)
(* ------------------------------------------------------------------------- *)

val gilithRepository : name

(* ------------------------------------------------------------------------- *)
(* Haskell export names.                                                     *)
(* ------------------------------------------------------------------------- *)

val haskell : name

val srcHaskellTheory : name

val testHaskellTheory : name

val exportHaskell : name -> name option

val isHaskell : name -> bool

(* ------------------------------------------------------------------------- *)
(* Export names.                                                             *)
(* ------------------------------------------------------------------------- *)

val isExport : name -> bool

end
