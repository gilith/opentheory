(* ========================================================================= *)
(* PACKAGE INFORMATION                                                       *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature PackageInformation =
sig

(* ------------------------------------------------------------------------- *)
(* Package information is stored in theory files.                            *)
(* ------------------------------------------------------------------------- *)

val mkFilename : {base : string} -> {filename : string}

val destFilename : {filename : string} -> {base : string} option

val isFilename : {filename : string} -> bool

(* ------------------------------------------------------------------------- *)
(* A type of package information.                                            *)
(* ------------------------------------------------------------------------- *)

type information

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype information' =
    Information' of
      {tags : PackageTag.tag list,
       theories : PackageTheory.theory list}

val mk : information' -> information

val dest : information -> information'

(* ------------------------------------------------------------------------- *)
(* Package information.                                                      *)
(* ------------------------------------------------------------------------- *)

val tags : information -> PackageTag.tag list

(* ------------------------------------------------------------------------- *)
(* Package name.                                                             *)
(* ------------------------------------------------------------------------- *)

val name : information -> PackageName.name

val version : information -> PackageVersion.version

val nameVersion : information -> PackageNameVersion.nameVersion

(* ------------------------------------------------------------------------- *)
(* Package description.                                                      *)
(* ------------------------------------------------------------------------- *)

val description : information -> {description : string}

(* ------------------------------------------------------------------------- *)
(* Package author.                                                           *)
(* ------------------------------------------------------------------------- *)

val author : information -> PackageAuthor.author

(* ------------------------------------------------------------------------- *)
(* Package license.                                                          *)
(* ------------------------------------------------------------------------- *)

val license : information -> {license : string}

(* ------------------------------------------------------------------------- *)
(* Extra package files.                                                      *)
(* ------------------------------------------------------------------------- *)

val extraFiles : information -> PackageExtra.extra list

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

val requires : information -> PackageName.name list

(* ------------------------------------------------------------------------- *)
(* Show.                                                                     *)
(* ------------------------------------------------------------------------- *)

val show : information -> Show.show

(* ------------------------------------------------------------------------- *)
(* Package theory graph.                                                     *)
(* ------------------------------------------------------------------------- *)

val theories : information -> PackageTheory.theory list

val emptyTheories : information -> bool

(* ------------------------------------------------------------------------- *)
(* Package articles.                                                         *)
(* ------------------------------------------------------------------------- *)

val articleFiles : information -> {filename : string} list

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val includes :
    information ->
    (PackageNameVersion.nameVersion * Checksum.checksum option) list

val nameVersionIncludes : information -> PackageNameVersionSet.set

val updateIncludes :
    (PackageNameVersion.nameVersion -> Checksum.checksum option ->
     (PackageNameVersion.nameVersion * Checksum.checksum option) option) ->
    information -> information option

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : information Print.pp

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parser : (char,information) Parse.parser

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

val fromTextFile : {filename : string} -> information

val toTextFile : {information : information, filename : string} -> unit

end
