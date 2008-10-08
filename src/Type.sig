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
(* Constructors and destructors                                              *)
(* ------------------------------------------------------------------------- *)

datatype ty' =
    TypeVar of Name.name
  | TypeOp of Name.name * ty list

val mk : ty' -> ty
val dest : ty -> ty'

val mkVar : Name.name -> ty
val destVar : ty -> Name.name
val isVar : ty -> bool
val equalVar : Name.name -> ty -> bool

val mkOp : Name.name * ty list -> ty
val destOp : ty -> Name.name * ty list
val isOp : ty -> bool

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : ty * ty -> order

val equal : ty -> ty -> bool

(* ------------------------------------------------------------------------- *)
(* Type variables.                                                           *)
(* ------------------------------------------------------------------------- *)

val alphaTy : ty

val typeVars : ty -> NameSet.set

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

val typeOps : ty -> NameSet.set

(* ------------------------------------------------------------------------- *)
(* Primitive types.                                                          *)
(* ------------------------------------------------------------------------- *)

val boolTy : ty

val mkFun : ty * ty -> ty
val destFun : ty -> ty * ty
val isFun : ty -> bool

(* ------------------------------------------------------------------------- *)
(* The type registry (initially contains the primitive type operators).      *)
(* ------------------------------------------------------------------------- *)

val typeArity : Name.name -> int option

val allTypes : unit -> Name.name list

val declareType : Name.name -> int -> unit

end
