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

val add : thms -> ObjectProv.object -> thms

val search : thms -> Sequent.sequent -> (Thm.thm * ObjectProv.object) option

val toThmSet : thms -> ThmSet.set

val toObjectSet : thms -> ObjectProvSet.set

end
