(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPE OPERATORS                                         *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature TypeOp =
sig

(* ------------------------------------------------------------------------- *)
(* A type of type operators.                                                 *)
(* ------------------------------------------------------------------------- *)

type opTy = TypeTerm.opTy

type opTyData =
     {name : Name.name,
      arity : int,
      prov : TypeTerm.provOpTy}

val mk : opTyData -> opTy

val dest : opTy -> opTyData

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val name : opTy -> Name.name

val arity : opTy -> int

val prov : opTy -> TypeTerm.provOpTy

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : opTy * opTy -> order

val equal : opTy -> opTy -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : opTy Print.pp

val toString : opTy -> string

end
