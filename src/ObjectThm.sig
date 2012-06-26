(* ========================================================================= *)
(* THEOREM OBJECTS                                                           *)
(* Copyright (c) 2011 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

signature ObjectThm =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theorem objects.                                                *)
(* ------------------------------------------------------------------------- *)

datatype thm =
    Thm of
      {proof : Object.object,
       hyp : Object.object,
       concl : Object.object}

(* ------------------------------------------------------------------------- *)
(* Converting to a real theorem.                                             *)
(* ------------------------------------------------------------------------- *)

val thm : thm -> Thm.thm

(* ------------------------------------------------------------------------- *)
(* Mapping over theorem objects.                                             *)
(* ------------------------------------------------------------------------- *)

val maps :
    (Object.object -> 's -> Object.object option * 's) ->
    thm -> 's -> thm option * 's

(* ------------------------------------------------------------------------- *)
(* Eliminate unwanted subterms.                                              *)
(* ------------------------------------------------------------------------- *)

val sharingEliminateUnwanted :
    thm -> ObjectUnwanted.eliminate -> thm option * ObjectUnwanted.eliminate

(* ------------------------------------------------------------------------- *)
(* Adding to a store.                                                        *)
(* ------------------------------------------------------------------------- *)

val addStore : ObjectStore.store -> thm -> ObjectStore.store

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : thm Print.pp

(* ------------------------------------------------------------------------- *)
(* A total order.                                                            *)
(* ------------------------------------------------------------------------- *)

val compare : thm * thm -> order

end
