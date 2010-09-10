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

val mkConst : Const.const * Type.ty -> term

val destConst : term -> Const.const * Type.ty

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

val rator : term -> term

val rand : term -> term

val land : term -> term

val listMkApp : term * term list -> term

val stripApp : term -> term * term list

(* Lambda abstractions *)

val mkAbs : Var.var * term -> term

val destAbs : term -> Var.var * term

val isAbs : term -> bool

val listMkAbs : Var.var list * term -> term

val stripAbs : term -> Var.var list * term

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

val addConstSharingConsts : Const.const -> sharingConsts -> sharingConsts

val addConstSetSharingConsts : ConstSet.set -> sharingConsts -> sharingConsts

val addSharingConsts : term -> sharingConsts -> sharingConsts

val addListSharingConsts : term list -> sharingConsts -> sharingConsts

val unionSharingConsts : sharingConsts -> sharingConsts -> sharingConsts

val toSetSharingConsts : sharingConsts -> ConstSet.set

val consts : term -> ConstSet.set

val constsList : term list -> ConstSet.set

(* ------------------------------------------------------------------------- *)
(* Type variables.                                                           *)
(* ------------------------------------------------------------------------- *)

type sharingTypeVars

val emptySharingTypeVars : sharingTypeVars

val addSharingTypeVars : term -> sharingTypeVars -> sharingTypeVars

val addListSharingTypeVars : term list -> sharingTypeVars -> sharingTypeVars

val toSetSharingTypeVars : sharingTypeVars -> NameSet.set

val typeVars : term -> NameSet.set

val typeVarsList : term list -> NameSet.set

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

type sharingTypeOps

val emptySharingTypeOps : sharingTypeOps

val addTypeOpSharingTypeOps : TypeOp.typeOp -> sharingTypeOps -> sharingTypeOps

val addTypeOpSetSharingTypeOps :
    TypeOpSet.set -> sharingTypeOps -> sharingTypeOps

val addTypeSharingTypeOps : Type.ty -> sharingTypeOps -> sharingTypeOps

val addSharingTypeOps : term -> sharingTypeOps -> sharingTypeOps

val addListSharingTypeOps : term list -> sharingTypeOps -> sharingTypeOps

val unionSharingTypeOps : sharingTypeOps -> sharingTypeOps -> sharingTypeOps

val toSetSharingTypeOps : sharingTypeOps -> TypeOpSet.set

val typeOps : term -> TypeOpSet.set

val typeOpsList : term list -> TypeOpSet.set

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

val lhs : term -> term

val rhs : term -> term

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype grammar =
    Grammar of
      {abs : Print.token,
       negations : Print.token list,
       infixes : Print.infixes,
       binders : Print.token list,
       ppToken : Print.token Print.pp,
       maximumSize : int}

val defaultGrammar : grammar

val ppWithGrammar : grammar -> Show.show -> term Print.pp

val ppWithShow : Show.show -> term Print.pp

val pp : term Print.pp

val toString : term -> string

val ppHtml : Show.show -> term Print.pp

end
