(* ========================================================================= *)
(* THEOREMS AND THEIR SYMBOLS                                                *)
(* Copyright (c) 2009 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Thms =
sig

(* ------------------------------------------------------------------------- *)
(* A type of object set theorems.                                            *)
(* ------------------------------------------------------------------------- *)

type thms

val empty : thms

val size : thms -> int

val thms : thms -> ThmSet.set

val symbol : thms -> Symbol.symbol

(* ------------------------------------------------------------------------- *)
(* Adding objects.                                                           *)
(* ------------------------------------------------------------------------- *)

val add : thms -> Thm.thm -> thms

val addList : thms -> Thm.thm list -> thms

val addSet : thms -> ThmSet.set -> thms

val union : thms -> thms -> thms

(* ------------------------------------------------------------------------- *)
(* Searching for theorems.                                                   *)
(* ------------------------------------------------------------------------- *)

val search : thms -> Sequent.sequent -> Thm.thm option

end
