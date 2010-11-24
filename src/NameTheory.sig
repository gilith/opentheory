(* ========================================================================= *)
(* NAMED THEORIES                                                            *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature NameTheory =
sig

(* ------------------------------------------------------------------------- *)
(* A type of named theories.                                                 *)
(* ------------------------------------------------------------------------- *)

type nameTheory

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk : PackageTheory.name * Theory.theory -> nameTheory

val name : nameTheory -> PackageTheory.name

val theory : nameTheory -> Theory.theory

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : nameTheory * nameTheory -> order

val equal : nameTheory -> nameTheory -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : nameTheory Print.pp

end
