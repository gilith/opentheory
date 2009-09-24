(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPES                                                  *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
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
(* Type IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type id = TypeTerm.idTy

val id : ty -> id

val equalId : id -> ty -> bool

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

(* ------------------------------------------------------------------------- *)
(* Type variables.                                                           *)
(* ------------------------------------------------------------------------- *)

type sharingTypeVars

val emptySharingTypeVars : sharingTypeVars

val addSharingTypeVars : sharingTypeVars -> ty list -> sharingTypeVars

val toSetSharingTypeVars : sharingTypeVars -> NameSet.set

val typeVarsList : ty list -> NameSet.set

val typeVars : ty -> NameSet.set

val alpha : ty

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

type sharingTypeOps

val emptySharingTypeOps : sharingTypeOps

val addSharingTypeOps : sharingTypeOps -> ty list -> sharingTypeOps

val toSetSharingTypeOps : sharingTypeOps -> TypeOpSet.set

val typeOpsList : ty list -> TypeOpSet.set

val typeOps : ty -> TypeOpSet.set

(* ------------------------------------------------------------------------- *)
(* Primitive types.                                                          *)
(* ------------------------------------------------------------------------- *)

(* Booleans *)

val nameBool : Name.name

val typeOpBool : TypeOp.typeOp

val bool : ty

val isBool : ty -> bool

(* Function spaces *)

val nameFun : Name.name

val typeOpFun : TypeOp.typeOp

val mkFun : ty * ty -> ty

val destFun : ty -> ty * ty

val isFun : ty -> bool

val domainFun : ty -> ty

val rangeFun : ty -> ty

val listMkFun : ty list * ty -> ty

val stripFun : ty -> ty list * ty

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val maximumSize : int ref

val pp : ty Print.pp

val toString : ty -> string

end
