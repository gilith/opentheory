(* ========================================================================= *)
(* HIGHER ORDER LOGIC TERMS                                                  *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Term =
sig

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic terms.                                       *)
(* ------------------------------------------------------------------------- *)

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

(* Constants *)

val mkConst : Name.name * Type.ty -> term
val destConst : term -> Name.name * Type.ty
val isConst : term -> bool

(* Variables *)

val mkVar : Var.var -> term
val destVar : term -> Var.var
val isVar : term -> bool
val equalVar : Var.var -> term -> bool

(* Function applications *)

val mkComb : term * term -> term
val destComb : term -> term * term
val isComb : term -> bool

(* Function abstractions *)

val mkAbs : Var.var * term -> term
val destAbs : term -> Var.var * term
val isAbs : term -> bool

(* ------------------------------------------------------------------------- *)
(* Term IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type termId = int

val id : term -> termId

val equalId : term -> term -> bool

(* ------------------------------------------------------------------------- *)
(* Number of constructors.                                                   *)
(* ------------------------------------------------------------------------- *)

val size : term -> int

(* ------------------------------------------------------------------------- *)
(* A total order on terms, with and without alpha equivalence.               *)
(* ------------------------------------------------------------------------- *)

val compare : term * term -> order
val equal : term -> term -> bool

val alphaCompare : term * term -> order
val alphaEqual : term -> term -> bool

(* ------------------------------------------------------------------------- *)
(* The type of a term.                                                       *)
(* ------------------------------------------------------------------------- *)

val typeOf : term -> Type.ty

(* ------------------------------------------------------------------------- *)
(* Free variables.                                                           *)
(* ------------------------------------------------------------------------- *)

val freeVars : term -> VarSet.set

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val consts : term -> NameSet.set

(* ------------------------------------------------------------------------- *)
(* Type variables.                                                           *)
(* ------------------------------------------------------------------------- *)

val typeVars : term -> NameSet.set

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

val typeOps : term -> NameSet.set

(* ------------------------------------------------------------------------- *)
(* Primitive constants.                                                      *)
(* ------------------------------------------------------------------------- *)

(* Equality *)

val eqType : Type.ty -> Type.ty
val eqTerm : term
val mkEq : term * term -> term
val destEq : term -> term * term
val isEq : term -> bool

(* ------------------------------------------------------------------------- *)
(* The constant registry (initially contains the primitive constants).       *)
(* ------------------------------------------------------------------------- *)

val declaredConst : Name.name -> Type.ty option

val allDeclared : unit -> NameSet.set

val declare : Name.name -> Type.ty -> unit

end
