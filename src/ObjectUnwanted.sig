(* ========================================================================= *)
(* UNWANTED OPENTHEORY OBJECTS                                               *)
(* Copyright (c) 2011 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature ObjectUnwanted =
sig

(* ------------------------------------------------------------------------- *)
(* The Unwanted namespace.                                                   *)
(* ------------------------------------------------------------------------- *)

val namespace : Namespace.namespace

(* ------------------------------------------------------------------------- *)
(* Unwanted constants.                                                       *)
(* ------------------------------------------------------------------------- *)

val idName : Name.name

(* ------------------------------------------------------------------------- *)
(* Eliminating Unwanted objects.                                             *)
(* ------------------------------------------------------------------------- *)

type eliminate

val new : {savable : bool} -> eliminate

(* Objects *)

val sharingEliminate :
    ObjectProv.object -> eliminate -> ObjectProv.object option * eliminate

val eliminate : eliminate -> ObjectProv.object -> ObjectProv.object option

end
