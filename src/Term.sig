(* ========================================================================= *)
(* HIGHER ORDER LOGIC TERMS                                                  *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Term =
sig

type term

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors                                              *)
(* ------------------------------------------------------------------------- *)

datatype term' =
    Const of Name.name * Type.ty
  | Var of Var.var
  | App of term * term
  | Lam of Var.var * term

val mk : term' -> term
val dest : term -> term'

val mk_const : Name.name * Type.ty -> term
val dest_const : term -> Name.name * Type.ty
val is_const : term -> bool

val mk_var : Var.var -> term
val dest_var : term -> Var.var
val is_var : term -> bool
val equal_var : Var.var -> term -> bool

val mk_comb : term * term -> term
val dest_comb : term -> term * term
val is_comb : term -> bool

val mk_abs : Var.var * term -> term
val dest_abs : term -> Var.var * term
val is_abs : term -> bool

(* ------------------------------------------------------------------------- *)
(* A total order on terms, with and without alpha equivalence                *)
(* ------------------------------------------------------------------------- *)

val compare : term * term -> order
val equal : term -> term -> bool

val alpha_compare : term * term -> order
val alpha_equal : term -> term -> bool

(* ------------------------------------------------------------------------- *)
(* Type checking                                                             *)
(* ------------------------------------------------------------------------- *)

val type_of : term -> Type.ty

(* ------------------------------------------------------------------------- *)
(* Free term and type variables                                              *)
(* ------------------------------------------------------------------------- *)

val type_vars : term -> NameSet.set

val free_vars : term -> VarSet.set

(* ------------------------------------------------------------------------- *)
(* Primitive constants                                                       *)
(* ------------------------------------------------------------------------- *)

(* Equality *)

val eq : term
val mk_eq : term * term -> term
val dest_eq : term -> term * term
val is_eq : term -> bool

(* Hilbert's indefinite choice operator (epsilon) *)

val select : term
val mk_select : Var.var * term -> term
val dest_select : term -> Var.var * term
val is_select : term -> bool

(* ------------------------------------------------------------------------- *)
(* The constant registry (initially contains the primitive constants)        *)
(* ------------------------------------------------------------------------- *)

val const_type : Name.name -> Type.ty

val all_consts : unit -> Name.name list

val declare_const : Name.name -> Type.ty -> unit

end
