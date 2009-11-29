(* ========================================================================= *)
(* SYMBOLS AND THEOREMS CONTAINED IN A SET OF OBJECTS                        *)
(* Copyright (c) 2004 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature ObjectThms =
sig

(* ------------------------------------------------------------------------- *)
(* A type of object set theorems.                                            *)
(* ------------------------------------------------------------------------- *)

type thms

val empty : thms

val size : thms -> {objs : int, thms : int}

val objects : thms -> ObjectProvSet.set

val symbol : thms -> Symbol.symbol

(* ------------------------------------------------------------------------- *)
(* Adding objects.                                                           *)
(* ------------------------------------------------------------------------- *)

val singleton : ObjectProv.object -> thms

val add : thms -> ObjectProv.object -> thms

val addList : thms -> ObjectProv.object list -> thms

val addSet : thms -> ObjectProvSet.set -> thms

val union : thms -> thms -> thms

(* ------------------------------------------------------------------------- *)
(* Searching for theorems.                                                   *)
(* ------------------------------------------------------------------------- *)

val search : thms -> Sequent.sequent -> (Thm.thm * ObjectProv.object) option

val toThmSet : thms -> ThmSet.set

(* ------------------------------------------------------------------------- *)
(* Building objects using object set theorems.                               *)
(* ------------------------------------------------------------------------- *)

val buildObject :
    {savable : bool} -> thms -> Object.object -> ObjectProv.object

end
