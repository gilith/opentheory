(* ========================================================================= *)
(* HIGHER ORDER LOGIC TERMS                                                  *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Term =
sig

type term

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype term' =
    Const of Name.name * Type.ty
  | Var of Var.var
  | Comb of term * term
  | Abs of Var.var * term

val mk : term' -> term
val dest : term -> term'

val mkConst : Name.name * Type.ty -> term
val destConst : term -> Name.name * Type.ty
val isConst : term -> bool

val mkVar : Var.var -> term
val destVar : term -> Var.var
val isVar : term -> bool
val equalVar : Var.var -> term -> bool

val mkComb : term * term -> term
val destComb : term -> term * term
val isComb : term -> bool

val mkAbs : Var.var * term -> term
val destAbs : term -> Var.var * term
val isAbs : term -> bool

(* ------------------------------------------------------------------------- *)
(* A total order on terms, with and without alpha equivalence.               *)
(* ------------------------------------------------------------------------- *)

val compare : term * term -> order
val equal : term -> term -> bool

val alphaCompare : term * term -> order
val alphaEqual : term -> term -> bool

(* ------------------------------------------------------------------------- *)
(* Type checking.                                                            *)
(* ------------------------------------------------------------------------- *)

val typeOf : term -> Type.ty

(* ------------------------------------------------------------------------- *)
(* Free term and type variables.                                             *)
(* ------------------------------------------------------------------------- *)

val typeVars : term -> NameSet.set

val freeVars : term -> VarSet.set

(* ------------------------------------------------------------------------- *)
(* Type operators and constants.                                             *)
(* ------------------------------------------------------------------------- *)

val typeOps : term -> NameSet.set

val consts : term -> NameSet.set

(* ------------------------------------------------------------------------- *)
(* Primitive constants.                                                      *)
(* ------------------------------------------------------------------------- *)

(* Equality *)

val eqTy : Type.ty -> Type.ty
val eqTm : term
val mkEq : term * term -> term
val destEq : term -> term * term
val isEq : term -> bool

(* ------------------------------------------------------------------------- *)
(* The constant registry (initially contains the primitive constants).       *)
(* ------------------------------------------------------------------------- *)

val constType : Name.name -> Type.ty option

val allConsts : unit -> Name.name list

val declareConst : Name.name -> Type.ty -> unit

end
