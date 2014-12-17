(* ========================================================================= *)
(* PACKAGE INFORMATION STORED AS "NAME: VALUE" TAGS                          *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature PackageTag =
sig

(* ------------------------------------------------------------------------- *)
(* A type of "NAME: VALUE" tags.                                             *)
(* ------------------------------------------------------------------------- *)

type name = PackageName.name

type value = string

type tag

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype tag' =
    Tag' of
      {name : name,
       value : value}

val mk : tag' -> tag

val dest : tag -> tag'

val name : tag -> name

val value : tag -> value

val equalName : name -> tag -> bool

val destName : name -> tag -> value option

val filterName : name -> tag list -> value list

val partitionName : name -> tag list -> value list * tag list

val getName : name -> tag list -> value  (* raises Error if non-unique *)

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : tag * tag -> order

val equal : tag -> tag -> bool

(* ------------------------------------------------------------------------- *)
(* Package basics.                                                           *)
(* ------------------------------------------------------------------------- *)

val findName : tag list -> PackageName.name

val findVersion : tag list -> PackageVersion.version

val findDescription : tag list -> {description : string}

val findAuthor : tag list -> PackageAuthor.author

val findLicense : tag list -> {license : string}

(* ------------------------------------------------------------------------- *)
(* Extra package files.                                                      *)
(* ------------------------------------------------------------------------- *)

val toExtra : tag -> PackageExtra.extra option

val fromExtra : PackageExtra.extra -> tag

val toExtraList : tag list -> PackageExtra.extra list

val fromExtraList : PackageExtra.extra list -> tag list

(* ------------------------------------------------------------------------- *)
(* Package requirements.                                                     *)
(* ------------------------------------------------------------------------- *)

val requires : tag list -> PackageName.name list

(* ------------------------------------------------------------------------- *)
(* Shows.                                                                    *)
(* ------------------------------------------------------------------------- *)

val toMapping : tag -> Show.mapping option

val fromMapping : Show.mapping -> tag

val toShow : tag list -> Show.show

val fromShow : Show.show -> tag list

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : tag Print.pp

val ppList : tag list Print.pp

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

val parser : (char,tag) Parse.parser

val parserList : (char, tag list) Parse.parser

end
