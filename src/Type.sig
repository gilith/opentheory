(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPES                                                  *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Type =
sig

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic types.                                       *)
(* ------------------------------------------------------------------------- *)

type ty

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype ty' =
    TypeVar of Name.name
  | TypeOp of Name.name * ty list

val mk : ty' -> ty
val dest : ty -> ty'

(* Variables *)

val mkVar : Name.name -> ty
val destVar : ty -> Name.name
val isVar : ty -> bool
val equalVar : Name.name -> ty -> bool

(* Operators *)

val mkOp : Name.name * ty list -> ty
val destOp : ty -> Name.name * ty list
val isOp : ty -> bool

(* ------------------------------------------------------------------------- *)
(* Type IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type tyId = int

val id : ty -> tyId

val equalId : ty -> ty -> bool

(* ------------------------------------------------------------------------- *)
(* Number of constructors.                                                   *)
(* ------------------------------------------------------------------------- *)

val size : ty -> int

val sizeList : ty list -> int

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : ty * ty -> order

val equal : ty -> ty -> bool

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

val toSetSharingTypeOps : sharingTypeOps -> NameSet.set

val typeOpsList : ty list -> NameSet.set

val typeOps : ty -> NameSet.set

(* ------------------------------------------------------------------------- *)
(* Primitive types.                                                          *)
(* ------------------------------------------------------------------------- *)

(* Booleans *)

val bool : ty

(* Function spaces *)

val mkFun : ty * ty -> ty
val destFun : ty -> ty * ty
val isFun : ty -> bool

(* ------------------------------------------------------------------------- *)
(* The type registry (initially contains the primitive type operators).      *)
(* ------------------------------------------------------------------------- *)

val declare : Name.name -> int -> unit

val declaredArity : Name.name -> int option

val allDeclared : unit -> NameSet.set

end
