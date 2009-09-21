(* ========================================================================= *)
(* HIGHER ORDER LOGIC TERMS                                                  *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Term =
sig

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic terms.                                       *)
(* ------------------------------------------------------------------------- *)

type term = TypeTerm.term

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

type term' = TypeTerm.term'

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

val mkApp : term * term -> term

val destApp : term -> term * term

val isApp : term -> bool

(* Lambda abstractions *)

val mkAbs : Var.var * term -> term

val destAbs : term -> Var.var * term

val isAbs : term -> bool

(* ------------------------------------------------------------------------- *)
(* Term IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type id = TypeTerm.id

val id : term -> id

val equalId : id -> term -> bool

(* ------------------------------------------------------------------------- *)
(* Number of constructors.                                                   *)
(* ------------------------------------------------------------------------- *)

val size : term -> int

val sizeList : term list -> int

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

type sharingFreeVars

val newSharingFreeVars : sharingFreeVars

val sharingFreeVars : term -> sharingFreeVars -> VarSet.set * sharingFreeVars

val freeVarsList : term list -> VarSet.set

val freeVars : term -> VarSet.set

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

type sharingConsts

val emptySharingConsts : sharingConsts

val addSharingConsts : sharingConsts -> term list -> sharingConsts

val toSetSharingConsts : sharingConsts -> ConstSet.set

val constsList : term list -> ConstSet.set

val consts : term -> ConstSet.set

(* ------------------------------------------------------------------------- *)
(* Type variables.                                                           *)
(* ------------------------------------------------------------------------- *)

type sharingTypeVars

val emptySharingTypeVars : sharingTypeVars

val addSharingTypeVars : sharingTypeVars -> term list -> sharingTypeVars

val toSetSharingTypeVars : sharingTypeVars -> NameSet.set

val typeVarsList : term list -> NameSet.set

val typeVars : term -> NameSet.set

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

type sharingTypeOps

val emptySharingTypeOps : sharingTypeOps

val addSharingTypeOps : sharingTypeOps -> term list -> sharingTypeOps

val toSetSharingTypeOps : sharingTypeOps -> TypeOpSet.set

val typeOpsList : term list -> TypeOpSet.set

val typeOps : term -> TypeOpSet.set

(* ------------------------------------------------------------------------- *)
(* Primitive constants.                                                      *)
(* ------------------------------------------------------------------------- *)

(* Equality *)

val mkEqTy : Type.ty -> Type.ty

val destEqTy : Type.ty -> Type.ty

val isEqTy : Type.ty -> bool

val nameEq : Name.name

val constEq : Const.const

val mkEq : term * term -> term

val destEq : term -> term * term

val isEq : term -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val maximumSize : int ref

val showTypes : bool ref

val pp : term Print.pp

val toString : term -> string

end
