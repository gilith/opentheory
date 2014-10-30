(* ========================================================================= *)
(* CONVERTING OBJECTS TO A GIVEN ARTICLE VERSION                             *)
(* Copyright (c) 2014 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature ObjectVersion =
sig

(* ------------------------------------------------------------------------- *)
(* A type of object conversions.                                             *)
(* ------------------------------------------------------------------------- *)

type convert

val new : ArticleVersion.version -> convert

(* ------------------------------------------------------------------------- *)
(* Convert to a given article version: return NONE for unchanged.            *)
(* ------------------------------------------------------------------------- *)

val sharingConvert : Object.object -> convert -> Object.object option * convert

val convert : convert -> Object.object -> Object.object option

end
