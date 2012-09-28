(* ========================================================================= *)
(* THEOREMS AND THEIR SYMBOLS                                                *)
(* Copyright (c) 2009 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Thms =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theorems and their symbols.                                     *)
(* ------------------------------------------------------------------------- *)

type thms

val empty : thms

val size : thms -> int

val thms : thms -> ThmSet.set

val symbol : thms -> SymbolTable.table

(* ------------------------------------------------------------------------- *)
(* Adding theorems.                                                          *)
(* ------------------------------------------------------------------------- *)

val add : thms -> Thm.thm -> thms

val addList : thms -> Thm.thm list -> thms

val addSet : thms -> ThmSet.set -> thms

val singleton : Thm.thm -> thms

val fromList : Thm.thm list -> thms

val fromSet : ThmSet.set -> thms

(* ------------------------------------------------------------------------- *)
(* Merging.                                                                  *)
(* ------------------------------------------------------------------------- *)

val union : thms -> thms -> thms

val unionList : thms list -> thms

(* ------------------------------------------------------------------------- *)
(* Searching for theorems.                                                   *)
(* ------------------------------------------------------------------------- *)

val peek : thms -> Sequent.sequent -> Thm.thm option

val search : thms -> Sequent.sequent -> Thm.thm option

end
