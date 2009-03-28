(* ========================================================================= *)
(* SAVED THEOREM OBJECTS                                                     *)
(* Copyright (c) 2004-2009 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature ObjectSaved =
sig

(* ------------------------------------------------------------------------- *)
(* A type of saved theorem objects.                                          *)
(* ------------------------------------------------------------------------- *)

type saved

val empty : saved

val thms : saved -> ObjectThms.thms

val union : saved -> saved -> saved

val add : saved -> ObjectProv.object -> saved

val search : saved -> Sequent.sequent -> Thm.thm option

end
