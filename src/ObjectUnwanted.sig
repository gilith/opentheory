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
    Object.object -> eliminate -> Object.object option * eliminate

val eliminate : eliminate -> Object.object -> Object.object option

end
