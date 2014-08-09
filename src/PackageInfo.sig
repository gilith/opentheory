(* ========================================================================= *)
(* PACKAGE INFORMATION                                                       *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature PackageInfo =
sig

(* ------------------------------------------------------------------------- *)
(* Package information is stored in theory files.                            *)
(* ------------------------------------------------------------------------- *)

val mkFilename : PackageName.name -> {filename : string}

val destFilename : {filename : string} -> PackageName.name option

val isFilename : {filename : string} -> bool

(* ------------------------------------------------------------------------- *)
(* A type of package information.                                            *)
(* ------------------------------------------------------------------------- *)

type info

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype info' =
    Info' of
      {tags : PackageTag.tag list,
       theories : PackageTheory.theory list}

val mk : info' -> info

val dest : info -> info'

(* ------------------------------------------------------------------------- *)
(* Package information.                                                      *)
(* ------------------------------------------------------------------------- *)

val tags : info -> PackageTag.tag list

(* ------------------------------------------------------------------------- *)
(* Package name.                                                             *)
(* ------------------------------------------------------------------------- *)

val name : info -> PackageName.name

val version : info -> PackageVersion.version

val nameVersion : info -> PackageNameVersion.nameVersion

(* ------------------------------------------------------------------------- *)
(* Package description.                                                      *)
(* ------------------------------------------------------------------------- *)

val description : info -> {description : string}

(* ------------------------------------------------------------------------- *)
(* Package author.                                                           *)
(* ------------------------------------------------------------------------- *)

val author : info -> PackageAuthor.author

(* ------------------------------------------------------------------------- *)
(* Package license.                                                          *)
(* ------------------------------------------------------------------------- *)

val license : info -> {license : string}

(* ------------------------------------------------------------------------- *)
(* Extra package files.                                                      *)
(* ------------------------------------------------------------------------- *)

val extraFiles : info -> PackageExtra.extra list

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

val requires : info -> PackageName.name list

(* ------------------------------------------------------------------------- *)
(* Show.                                                                     *)
(* ------------------------------------------------------------------------- *)

val show : info -> Show.show

(* ------------------------------------------------------------------------- *)
(* Package theory.                                                           *)
(* ------------------------------------------------------------------------- *)

val theory : info -> PackageTheory.theory list

val emptyTheory : info -> bool

(* ------------------------------------------------------------------------- *)
(* Package articles.                                                         *)
(* ------------------------------------------------------------------------- *)

val articles : info -> {filename : string} list

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val includes :
    info ->
    (PackageNameVersion.nameVersion * Checksum.checksum option) list

val updateIncludes :
    (PackageNameVersion.nameVersion -> Checksum.checksum option ->
     (PackageNameVersion.nameVersion * Checksum.checksum option) option) ->
    info -> info option

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : info Print.pp

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parser : (char,info) Parse.parser

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

val fromTextFile : {filename : string} -> info

val toTextFile : {info : info, filename : string} -> unit

end
