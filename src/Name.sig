(* ========================================================================= *)
(* OPENTHEORY NAMES                                                          *)
(* Copyright (c) 2004 Joe Hurd, distributed under the MIT license            *)
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

(* Bytes *)

val byteTypeOp : name

(* Lists *)

val listTypeOp : name

(* Options *)

val optionTypeOp : name

(* Pairs *)

val pairTypeOp : name

(* 16-bit words *)

val word16TypeOp : name

(* Natural numbers *)

val naturalTypeOp : name

(* Random streams *)

val randomTypeOp : name

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

(* Bytes *)

val addByteConst : name
val andByteConst : name
val bitByteConst : name
val fromNaturalByteConst : name
val leByteConst : name
val ltByteConst : name
val multiplyByteConst : name
val notByteConst : name
val orByteConst : name
val shiftLeftByteConst : name
val shiftRightByteConst : name
val subtractByteConst : name

(* Functions *)

val composeConst : name
val idConst : name

(* Lists *)

val appendConst : name
val consConst : name
val lengthConst : name
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
val divConst : name
val isFromNaturalConst : name -> bool
val leConst : name
val ltConst : name
val minimalConst : name
val modConst : name
val multiplyConst : name
val subtractConst : name
val sucConst : name
val zeroConst : name

(* Random streams *)

val bitConst : name
val splitConst : name

(* Sets *)

val emptyConst : name
val differenceConst : name
val fromPredicateConst : name
val intersectConst : name
val memberConst : name
val properSubsetConst : name
val subsetConst : name
val unionConst : name

(* 16-bit words *)

val addWord16Const : name
val andWord16Const : name
val bitWord16Const : name
val fromBytesWord16Const : name
val fromNaturalWord16Const : name
val leWord16Const : name
val ltWord16Const : name
val multiplyWord16Const : name
val notWord16Const : name
val orWord16Const : name
val shiftLeftWord16Const : name
val shiftRightWord16Const : name
val subtractWord16Const : name
val toBytesWord16Const : name

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
