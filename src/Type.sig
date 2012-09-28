(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPES                                                  *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Type =
sig

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic types.                                       *)
(* ------------------------------------------------------------------------- *)

type ty = TypeTerm.ty

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

type ty' = TypeTerm.ty'

val mk : ty' -> ty

val dest : ty -> ty'

(* Variables *)

val mkVar : Name.name -> ty

val destVar : ty -> Name.name

val isVar : ty -> bool

val equalVar : Name.name -> ty -> bool

(* Operators *)

val mkOp : TypeOp.typeOp * ty list -> ty

val destOp : ty -> TypeOp.typeOp * ty list

val isOp : ty -> bool

val destOpTy : TypeOp.typeOp -> ty -> ty list

val isOpTy : TypeOp.typeOp -> ty -> bool

(* ------------------------------------------------------------------------- *)
(* Number of constructors.                                                   *)
(* ------------------------------------------------------------------------- *)

val size : ty -> int

val sizeList : ty list -> int

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : ty * ty -> order

val compareList : ty list * ty list -> order

val equal : ty -> ty -> bool

val equalList : ty list -> ty list -> bool

val checkEqual : (TypeTerm.term -> TypeTerm.term -> unit) -> ty -> ty -> unit

(* ------------------------------------------------------------------------- *)
(* Type variables.                                                           *)
(* ------------------------------------------------------------------------- *)

type sharingTypeVars

val emptySharingTypeVars : sharingTypeVars

val addSharingTypeVars : ty -> sharingTypeVars -> sharingTypeVars

val addListSharingTypeVars : ty list -> sharingTypeVars -> sharingTypeVars

val toSetSharingTypeVars : sharingTypeVars -> NameSet.set

val typeVars : ty -> NameSet.set

val typeVarsList : ty list -> NameSet.set

val alpha : ty

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

type sharingTypeOps

val emptySharingTypeOps : sharingTypeOps

val addTypeOpSharingTypeOps : TypeOp.typeOp -> sharingTypeOps -> sharingTypeOps

val addTypeOpSetSharingTypeOps :
    TypeOpSet.set -> sharingTypeOps -> sharingTypeOps

val unionSharingTypeOps : sharingTypeOps -> sharingTypeOps -> sharingTypeOps

val addSharingTypeOps : ty -> sharingTypeOps -> sharingTypeOps

val addListSharingTypeOps : ty list -> sharingTypeOps -> sharingTypeOps

val toSetSharingTypeOps : sharingTypeOps -> TypeOpSet.set

val typeOps : ty -> TypeOpSet.set

val typeOpsList : ty list -> TypeOpSet.set

(* ------------------------------------------------------------------------- *)
(* Primitive type operators.                                                 *)
(* ------------------------------------------------------------------------- *)

(* Booleans *)

val bool : ty

val isBool : ty -> bool

(* Function spaces *)

val mkFun : ty * ty -> ty

val destFun : ty -> ty * ty

val isFun : ty -> bool

val domainFun : ty -> ty

val rangeFun : ty -> ty

val listMkFun : ty list * ty -> ty

val stripFun : ty -> ty list * ty

(* Individuals *)

val ind : ty

val isInd : ty -> bool

(* ------------------------------------------------------------------------- *)
(* Primitive constants.                                                      *)
(* ------------------------------------------------------------------------- *)

(* Equality *)

val mkEq : ty -> ty

val destEq : ty -> ty

val isEq : ty -> bool

val boolEq : ty

val isBoolEq : ty -> bool

(* Hilbert's choice operator *)

val mkSelect : ty -> ty

val destSelect : ty -> ty

val isSelect : ty -> bool

(* ------------------------------------------------------------------------- *)
(* General syntax operations.                                                *)
(* ------------------------------------------------------------------------- *)

(* Nullary operators *)

val isNullaryOp : (TypeOp.typeOp -> bool) -> ty -> bool

(* Unary operators *)

val destUnaryOp : (TypeOp.typeOp -> bool) -> ty -> ty

(* Binary operators *)

val destBinaryOp : (TypeOp.typeOp -> bool) -> ty -> ty * ty

val stripBinaryOp : (TypeOp.typeOp -> bool) -> ty -> ty list * ty

(* Ternary operators *)

val destTernaryOp : (TypeOp.typeOp -> bool) -> ty -> ty * ty * ty

(* ------------------------------------------------------------------------- *)
(* Special syntax.                                                           *)
(* ------------------------------------------------------------------------- *)

(* Lists *)

val destList : ty -> ty

val isList : ty -> bool

(* Pairs *)

val destPair : ty -> ty * ty

val isPair : ty -> bool

(* Random streams *)

val isRandom : ty -> bool

(* Streams *)

val destStream : ty -> ty

val isStream : ty -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {infixes : Print.infixes,
       showTypeOp : Show.show -> TypeOp.typeOp * int -> Name.name,
       ppVar : Name.name Print.pp,
       ppTypeOp : Show.show -> (TypeOp.typeOp * int) Print.pp,
       ppInfix : Show.show -> TypeOp.typeOp Print.pp,
       maximumSize : int}

val defaultGrammar : grammar

val ppWithGrammar : grammar -> Show.show -> ty Print.pp

val ppWithShow : Show.show -> ty Print.pp

val pp : ty Print.pp

val toString : ty -> string

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

val htmlGrammar : grammar

val ppHtml : Show.show -> ty Print.pp

(* ------------------------------------------------------------------------- *)
(* Type IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type id = TypeTerm.idTy

val id : ty -> id

val equalId : id -> ty -> bool

end
