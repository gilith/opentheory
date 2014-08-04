(* ========================================================================= *)
(* PACKAGE THEORY SOURCE FILES                                               *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature PackageSource =
sig

(* ------------------------------------------------------------------------- *)
(* Theory package source filenames.                                          *)
(* ------------------------------------------------------------------------- *)

val mkFilename : PackageName.name -> {filename : string}

val destFilename : {filename : string} -> PackageName.name option

val isFilename : {filename : string} -> bool

(* ------------------------------------------------------------------------- *)
(* A type of theory package source files.                                    *)
(* ------------------------------------------------------------------------- *)

type source

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype source' =
    Source' of
      {tags : PackageTag.tag list,
       theories : PackageTheory.theory list}

val mk : source' -> source

val dest : source -> source'

(* ------------------------------------------------------------------------- *)
(* Package information.                                                      *)
(* ------------------------------------------------------------------------- *)

val tags : source -> PackageTag.tag list

(* ------------------------------------------------------------------------- *)
(* Package name.                                                             *)
(* ------------------------------------------------------------------------- *)

val name : source -> PackageName.name

val version : source -> PackageVersion.version

val nameVersion : source -> PackageNameVersion.nameVersion

(* ------------------------------------------------------------------------- *)
(* Package description.                                                      *)
(* ------------------------------------------------------------------------- *)

val description : source -> {description : string}

(* ------------------------------------------------------------------------- *)
(* Package author.                                                           *)
(* ------------------------------------------------------------------------- *)

val author : source -> PackageAuthor.author

(* ------------------------------------------------------------------------- *)
(* Package license.                                                          *)
(* ------------------------------------------------------------------------- *)

val license : source -> {license : string}

(* ------------------------------------------------------------------------- *)
(* Extra package files.                                                      *)
(* ------------------------------------------------------------------------- *)

val extraFiles : source -> PackageExtra.extra list

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

val requires : source -> PackageName.name list

(* ------------------------------------------------------------------------- *)
(* Show.                                                                     *)
(* ------------------------------------------------------------------------- *)

val show : source -> Show.show

(* ------------------------------------------------------------------------- *)
(* Package theory.                                                           *)
(* ------------------------------------------------------------------------- *)

val theory : source -> PackageTheory.theory list

val emptyTheory : source -> bool

(* ------------------------------------------------------------------------- *)
(* Package articles.                                                         *)
(* ------------------------------------------------------------------------- *)

val articles : source -> {filename : string} list

(* ------------------------------------------------------------------------- *)
(* Package dependencies.                                                     *)
(* ------------------------------------------------------------------------- *)

val includes :
    source ->
    (PackageNameVersion.nameVersion * Checksum.checksum option) list

val updateIncludes :
    (PackageNameVersion.nameVersion -> Checksum.checksum option ->
     (PackageNameVersion.nameVersion * Checksum.checksum option) option) ->
    source -> source option

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : source Print.pp

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parser : (char,source) Parse.parser

(* ------------------------------------------------------------------------- *)
(* Input/Output.                                                             *)
(* ------------------------------------------------------------------------- *)

val fromTextFile : {filename : string} -> source

val toTextFile : {source : source, filename : string} -> unit

end
