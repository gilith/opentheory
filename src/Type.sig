(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPES                                                  *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Type =
sig

type ty

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors                                              *)
(* ------------------------------------------------------------------------- *)

datatype ty' = Type_var of Name.name | Type_op of Name.name * ty list;

val mk : ty' -> ty
val dest : ty -> ty'

val mk_var : Name.name -> ty
val dest_var : ty -> Name.name
val is_var : ty -> bool
val equal_var : Name.name -> ty -> bool

val mk_op : Name.name * ty list -> ty
val dest_op : ty -> Name.name * ty list
val is_op : ty -> bool

(* ------------------------------------------------------------------------- *)
(* A total order                                                             *)
(* ------------------------------------------------------------------------- *)

val compare : ty * ty -> order

val equal : ty -> ty -> bool

(* ------------------------------------------------------------------------- *)
(* Type variables                                                            *)
(* ------------------------------------------------------------------------- *)

val type_vars : ty -> NameSet.set

(* ------------------------------------------------------------------------- *)
(* Primitive types                                                           *)
(* ------------------------------------------------------------------------- *)

val alpha : ty

val bool : ty

val mk_fun : ty * ty -> ty
val dest_fun : ty -> ty * ty
val is_fun : ty -> bool

val ind : ty

(* ------------------------------------------------------------------------- *)
(* The type registry (initially contains the primitive type operators)       *)
(* ------------------------------------------------------------------------- *)

val type_arity : Name.name -> int

val all_types : unit -> Name.name list

val declare_type : Name.name -> int -> unit

end
