(* ========================================================================= *)
(* REPLACE DEFINITIONS WITH THEORY ASSUMPTIONS IN OPENTHEORY OBJECTS         *)
(* Copyright (c) 2016 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature ObjectSkipDefinitions =
sig

(* ------------------------------------------------------------------------- *)
(* Skip definitions.                                                         *)
(* ------------------------------------------------------------------------- *)

type skipDefinitions

val empty : skipDefinitions

(* Objects *)

val sharingSkipDefinitions :
    Object.object -> skipDefinitions -> Object.object option * skipDefinitions

val skipDefinitions : skipDefinitions -> Object.object -> Object.object option

end
