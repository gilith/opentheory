(* ========================================================================= *)
(* THEOREMS CONTAINED IN A SET OF OBJECTS                                    *)
(* Copyright (c) 2004-2009 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature ObjectThms =
sig

(* ------------------------------------------------------------------------- *)
(* A type of object set theorems.                                            *)
(* ------------------------------------------------------------------------- *)

type thms

val empty : thms

val size : thms -> {objs : int, thms : int}

val add : thms -> ObjectProv.object -> thms

val addList : thms -> ObjectProv.object list -> thms

val addSet : thms -> ObjectProvSet.set -> thms

val search : thms -> Sequent.sequent -> (Thm.thm * ObjectProv.object) option

val toObjectSet : thms -> ObjectProvSet.set

val toThmSet : thms -> ThmSet.set

end
