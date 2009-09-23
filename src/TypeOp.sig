(* ========================================================================= *)
(* HIGHER ORDER LOGIC TYPE OPERATORS                                         *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature TypeOp =
sig

(* ------------------------------------------------------------------------- *)
(* A type of type operators.                                                 *)
(* ------------------------------------------------------------------------- *)

type typeOp = TypeTerm.opTy

type typeOpData =
     {name : Name.name,
      arity : int,
      prov : TypeTerm.provOpTy}

val mk : typeOpData -> typeOp

val dest : typeOp -> typeOpData

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val name : typeOp -> Name.name

val arity : typeOp -> int

val prov : typeOp -> TypeTerm.provOpTy

val mkUndef : {name : Name.name, arity : int} -> typeOp

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : typeOp * typeOp -> order

val equal : typeOp -> typeOp -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : typeOp Print.pp

val toString : typeOp -> string

end
