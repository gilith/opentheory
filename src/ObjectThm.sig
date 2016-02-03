(* ========================================================================= *)
(* THEOREM OBJECTS                                                           *)
(* Copyright (c) 2011 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature ObjectThm =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theorem objects.                                                *)
(* ------------------------------------------------------------------------- *)

type thm

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

datatype thm' =
    Thm of
      {proof : Object.object,
       hyp : Object.object,
       concl : Object.object}

val mk : thm' -> thm

val dest : thm -> thm'

val proof : thm -> Object.object

val hyp : thm -> Object.object

val concl : thm -> Object.object

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
(* Rename symbol names.                                                      *)
(* ------------------------------------------------------------------------- *)

val sharingRename :
    thm -> ObjectRename.rename -> thm option * ObjectRename.rename

(* ------------------------------------------------------------------------- *)
(* Replace definitions with theory assumptions.                              *)
(* ------------------------------------------------------------------------- *)

val sharingSkipDefinitions :
    thm -> ObjectSkipDefinitions.skipDefinitions ->
    thm option * ObjectSkipDefinitions.skipDefinitions

(* ------------------------------------------------------------------------- *)
(* Adding to a store.                                                        *)
(* ------------------------------------------------------------------------- *)

val addStore : ObjectStore.store -> thm -> ObjectStore.store

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : thm Print.pp

end
