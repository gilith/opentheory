(* ========================================================================= *)
(* PACKAGE NAME/VERSIONS                                                     *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature PackageNameVersion =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory package name/versions.                                   *)
(* ------------------------------------------------------------------------- *)

type nameVersion

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype nameVersion' =
    NameVersion' of
      {name : PackageName.name,
       version : PackageVersion.version}

val mk : nameVersion' -> nameVersion

val dest : nameVersion -> nameVersion'

val name : nameVersion -> PackageName.name

val version : nameVersion -> PackageVersion.version

val equalName : PackageName.name -> nameVersion -> bool

val equalVersion : PackageVersion.version -> nameVersion -> bool

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : nameVersion * nameVersion -> order

val equal : nameVersion -> nameVersion -> bool

(* ------------------------------------------------------------------------- *)
(* Prefix names.                                                             *)
(* ------------------------------------------------------------------------- *)

val isPrefixName : nameVersion -> nameVersion -> bool

val isStrictPrefixName : nameVersion -> nameVersion -> bool

(* ------------------------------------------------------------------------- *)
(* Converting to a logic name.                                               *)
(* ------------------------------------------------------------------------- *)

val toGlobal : nameVersion -> Name.name

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : nameVersion Print.pp

val toString : nameVersion -> string

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parser : (char,nameVersion) Parse.parser

val fromString : string -> nameVersion

end
