(* ========================================================================= *)
(* HIGHER ORDER LOGIC CONSTANTS                                              *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
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

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : const * const -> order

val equal : const -> const -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : const Print.pp

val toString : const -> string

end
