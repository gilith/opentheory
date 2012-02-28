(* ========================================================================= *)
(* OPENTHEORY NAMES                                                          *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Name =
sig

(* ------------------------------------------------------------------------- *)
(* A type of names.                                                          *)
(* ------------------------------------------------------------------------- *)

type name

val mk : Namespace.namespace * Namespace.component -> name

val dest : name -> Namespace.namespace * Namespace.component

val namespace : name -> Namespace.namespace

val component : name -> Namespace.component

val toNamespace : name -> Namespace.namespace

val fromNamespace : Namespace.namespace -> name

(* ------------------------------------------------------------------------- *)
(* The top level namespace.                                                  *)
(* ------------------------------------------------------------------------- *)

val mkGlobal : Namespace.component -> name

val destGlobal : name -> Namespace.component

val isGlobal : name -> bool

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

val compare : name * name -> order

val equal : name -> name -> bool

val equalList : name list -> name list -> bool

(* ------------------------------------------------------------------------- *)
(* Fresh names.                                                              *)
(* ------------------------------------------------------------------------- *)

val newName : unit -> name

val newNames : int -> name list

val variantPrime : {avoid : name -> bool} -> name -> name

val variantNum : {avoid : name -> bool} -> name -> name

(* ------------------------------------------------------------------------- *)
(* Rewriting names.                                                          *)
(* ------------------------------------------------------------------------- *)

val rewrite : Namespace.namespace * Namespace.namespace -> name -> name option

(* ------------------------------------------------------------------------- *)
(* Characters.                                                               *)
(* ------------------------------------------------------------------------- *)

val firstChar : name -> char option

val lastChar : name -> char option

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

(* Primitive *)

val boolTypeOp : name
val funTypeOp : name
val indTypeOp : name

(* Lists *)

val listTypeOp : name

(* Options *)

val optionTypeOp : name

(* Pairs *)

val pairTypeOp : name

(* Natural numbers *)

val naturalTypeOp : name

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

(* Primitive *)

val eqConst : name
val selectConst : name

(* Boolean *)

val condConst : name
val conjConst : name
val disjConst : name
val existsConst : name
val existsUniqueConst : name
val falseConst : name
val forallConst : name
val impConst : name
val negConst : name
val trueConst : name

(* Functions *)

val composeConst : name
val idConst : name

(* Lists *)

val consConst : name
val nilConst : name

(* Options *)

val noneConst : name
val someConst : name

(* Pairs *)

val pairConst : name

(* Natural numbers *)

val addConst : name
val bit0Const : name
val bit1Const : name
val isFromNaturalConst : name -> bool
val minimalConst : name
val sucConst : name
val zeroConst : name

(* Sets *)

val emptyConst : name
val differenceConst : name
val fromPredicateConst : name
val intersectConst : name
val memberConst : name
val properSubsetConst : name
val subsetConst : name
val unionConst : name

(* Case expressions *)

val destCase : name -> name * name list

val isCase : name -> bool

(* ------------------------------------------------------------------------- *)
(* Parsing and pretty printing.                                              *)
(* ------------------------------------------------------------------------- *)

val pp : name Print.pp

val ppList : name list Print.pp

val toString : name -> string

val toStringList : name list -> string

val toHtml : name -> Html.inline list

val fromString : string -> name

(* The following can be used for serializing and unserializing *)

val ppQuoted : name Print.pp

val quotedToString : name -> string

val quotedParser : (char,name) Parse.parser

end
