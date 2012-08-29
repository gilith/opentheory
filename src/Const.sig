(* ========================================================================= *)
(* HIGHER ORDER LOGIC CONSTANTS                                              *)
(* Copyright (c) 2009 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

signature Const =
sig

(* ------------------------------------------------------------------------- *)
(* A type of constants.                                                      *)
(* ------------------------------------------------------------------------- *)

type const = TypeTerm.const

type constData =
     {name : Name.name,
      prov : TypeTerm.provConst}

val mk : constData -> const

val dest : const -> constData

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val name : const -> Name.name

val prov : const -> TypeTerm.provConst

val mkUndef : Name.name -> const

val isUndef : const -> bool

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : const * const -> order

val equal : const -> const -> bool

val checkEqual :
    (TypeTerm.term -> TypeTerm.term -> unit) -> const -> const -> unit

(* ------------------------------------------------------------------------- *)
(* Reconstructing the type from the provenance.                              *)
(* ------------------------------------------------------------------------- *)

val typeOf : const -> Type.ty option

(* ------------------------------------------------------------------------- *)
(* Primitive constants.                                                      *)
(* ------------------------------------------------------------------------- *)

(* Equality *)

val eq : const

val isEq : const -> bool

(* Hilbert's choice operator *)

val select : const

val isSelect : const -> bool

(* The standard primitives *)

val primitives : const list

(* ------------------------------------------------------------------------- *)
(* Special syntax.                                                           *)
(* ------------------------------------------------------------------------- *)

(* Boolean *)

val isCond : const -> bool

val isConj : const -> bool

val isDisj : const -> bool

val isExists : const -> bool

val isExistsUnique : const -> bool

val isFalse : const -> bool

val isForall : const -> bool

val isImp : const -> bool

val isNeg : const -> bool

val isTrue : const -> bool

(* Pairs *)

val isPair : const -> bool

(* Natural numbers *)

val isBit0 : const -> bool

val isBit1 : const -> bool

val isFromNatural : const -> bool

val isZero : const -> bool

(* Sets *)

val isFromPredicate : const -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val showName : Show.show -> const * Type.ty option -> Name.name

val ppWithShow : Show.show -> const Print.pp

val pp : const Print.pp

val toString : const -> string

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

val showNameHtml : Show.show -> const * Type.ty option -> Name.name

val toHtml : Show.show -> const * Type.ty option -> Html.inline list

end
