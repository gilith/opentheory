(* ========================================================================= *)
(* PACKAGE INFORMATION STORED AS NAME/VALUE TAGS                             *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageTag =
sig

(* ------------------------------------------------------------------------- *)
(* A type of tag names.                                                      *)
(* ------------------------------------------------------------------------- *)

type name = PackageName.name

val toStringName : name -> string

(* ------------------------------------------------------------------------- *)
(* A type of tag values.                                                     *)
(* ------------------------------------------------------------------------- *)

type value = string

val toStringValue : value -> string

(* ------------------------------------------------------------------------- *)
(* A type of tags.                                                           *)
(* ------------------------------------------------------------------------- *)

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

val destName : name -> tag -> value option

val filterName : name -> tag list -> value list

val getName : name -> tag list -> value

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

val findDescription : tag list -> string

val findAuthor : tag list -> string

val findLicense : tag list -> string

(* ------------------------------------------------------------------------- *)
(* Extra package files.                                                      *)
(* ------------------------------------------------------------------------- *)

val toExtra : tag -> PackageExtra.extra option

val fromExtra : PackageExtra.extra -> tag

val toExtraList : tag list -> PackageExtra.extra list

val fromExtraList : PackageExtra.extra list -> tag list

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
