(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPES AND TERMS                                        *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature TypeTerm =
sig

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic types.                                       *)
(* ------------------------------------------------------------------------- *)

type ty

(* ------------------------------------------------------------------------- *)
(* Type IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type idTy = int

val idTy : ty -> idTy

val equalIdTy : idTy -> ty -> bool

(* ------------------------------------------------------------------------- *)
(* The size of a type as the number of constructors.                         *)
(* ------------------------------------------------------------------------- *)

val sizeTy : ty -> int

val sizeListTy : ty list -> int

(* ------------------------------------------------------------------------- *)
(* A total order on types.                                                   *)
(* ------------------------------------------------------------------------- *)

val compareTy : ty * ty -> order

val equalTy : ty -> ty -> bool

(* ------------------------------------------------------------------------- *)
(* A type of higher order logic terms.                                       *)
(* ------------------------------------------------------------------------- *)

type term

(* ------------------------------------------------------------------------- *)
(* Term IDs.                                                                 *)
(* ------------------------------------------------------------------------- *)

type id = int

val id : term -> id

val equalId : id -> term -> bool

(* ------------------------------------------------------------------------- *)
(* The size of a term as the number of constructors.                         *)
(* ------------------------------------------------------------------------- *)

val size : term -> int

val sizeList : term list -> int

(* ------------------------------------------------------------------------- *)
(* The type of a term.                                                       *)
(* ------------------------------------------------------------------------- *)

val typeOf : term -> ty

(* ------------------------------------------------------------------------- *)
(* A total order on terms.                                                   *)
(* ------------------------------------------------------------------------- *)

val compare : term * term -> order

val equal : term -> term -> bool

(* ------------------------------------------------------------------------- *)
(* Type operator definitions.                                                *)
(* ------------------------------------------------------------------------- *)

datatype defOpTy =
    DefOpTy of
      {pred : term,
       vars : Name.name list}

(* Total order *)

val compareDefOpTy : defOpTy * defOpTy -> order

val equalDefOpTy : defOpTy -> defOpTy -> bool

(* ------------------------------------------------------------------------- *)
(* Type operator provenance.                                                 *)
(* ------------------------------------------------------------------------- *)

datatype provOpTy =
    UndefProvOpTy
  | DefProvOpTy of defOpTy

(* Total order *)

val compareProvOpTy : provOpTy * provOpTy -> order

val equalProvOpTy : provOpTy -> provOpTy -> bool

(* ------------------------------------------------------------------------- *)
(* Type operators.                                                           *)
(* ------------------------------------------------------------------------- *)

datatype opTy = OpTy of Name.name * provOpTy

(* Total order *)

val compareOpTy : opTy * opTy -> order

val equalOpTy : opTy -> opTy -> bool

(* ------------------------------------------------------------------------- *)
(* Type constructors and destructors.                                        *)
(* ------------------------------------------------------------------------- *)

datatype ty' =
    VarTy' of Name.name
  | OpTy' of opTy * ty list

val infoTy : ty -> {id : idTy, ty : ty', sz : int}

val mkTy : ty' -> ty

val destTy : ty -> ty'

(* Number of constructors *)

val sizeTy' : ty' -> int

(* Total order *)

val compareTy' : ty' * ty' -> order

val equalTy' : ty' -> ty' -> bool

(* ------------------------------------------------------------------------- *)
(* Primitive type operators.                                                 *)
(* ------------------------------------------------------------------------- *)

(* Booleans *)

val nameBool : Name.name

val bool : ty

val isBool : ty -> bool

(* Function spaces *)

val nameFun : Name.name

val mkFun : ty * ty -> ty

val destFun : ty -> ty * ty

val isFun : ty -> bool

(* ------------------------------------------------------------------------- *)
(* Variables.                                                                *)
(* ------------------------------------------------------------------------- *)

datatype var = Var of Name.name * ty

(* ------------------------------------------------------------------------- *)
(* Constant definitions.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype defConst = DefConst of term

(* Total order *)

val compareDefConst : defConst * defConst -> order

val equalDefConst : defConst -> defConst -> bool

(* ------------------------------------------------------------------------- *)
(* Constant provenance.                                                      *)
(* ------------------------------------------------------------------------- *)

datatype provConst =
    UndefProvConst
  | DefProvConst of defConst
  | AbsProvConst of defOpTy
  | RepProvConst of defOpTy

(* Total order *)

val compareProvConst : provConst * provConst -> order

val equalProvConst : provConst -> provConst -> bool

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

datatype const = Const of Name.name * provConst

(* Total order *)

val compareConst : const * const -> order

val equalConst : const -> const -> bool

(* ------------------------------------------------------------------------- *)
(* Term constructors and destructors.                                        *)
(* ------------------------------------------------------------------------- *)

datatype term' =
    Const' of const * ty
  | Var' of var
  | App' of term * term
  | Abs' of var * term

val info : term -> {id : id, tm : term', sz : int, ty : ty}

val mk : term' -> term

val dest : term -> term'

(* Number of constructors *)

val size' : term' -> int

(* Type *)

val typeOf' : term' -> ty

(* Total order *)

val compare' : term' * term' -> order

val equal' : term' -> term' -> bool

(* ------------------------------------------------------------------------- *)
(* Primitive constants.                                                      *)
(* ------------------------------------------------------------------------- *)

(* Equality *)

val nameEq : Name.name

val mkEq : term * term -> term

val destEq : term -> term * term

val isEq : term -> bool

end
