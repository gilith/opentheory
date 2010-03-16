(* ========================================================================= *)
(* IMPORTING NAMESPACES FOR PRINTING PURPOSES                                *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Show =
sig

(* ------------------------------------------------------------------------- *)
(* A type of mappings.                                                       *)
(* ------------------------------------------------------------------------- *)

datatype mapping =
    NamespaceMapping of Namespace.namespace * Namespace.namespace

val toStringMapping : mapping -> string

val fromStringMapping : string -> mapping

(* ------------------------------------------------------------------------- *)
(* A type of mapping collections.                                            *)
(* ------------------------------------------------------------------------- *)

type show

val toList : show -> mapping list

(* ------------------------------------------------------------------------- *)
(* The empty mapping.                                                        *)
(* ------------------------------------------------------------------------- *)

val natural : show

(* ------------------------------------------------------------------------- *)
(* Adding mappings.                                                          *)
(* ------------------------------------------------------------------------- *)

val add : show -> mapping -> show

val addList : show -> mapping list -> show

val fromList : mapping list -> show

(* ------------------------------------------------------------------------- *)
(* Mapping names.                                                            *)
(* ------------------------------------------------------------------------- *)

val showNamespace : show -> Namespace.namespace -> Namespace.namespace

val showName : show -> Name.name -> Name.name

(* ------------------------------------------------------------------------- *)
(* Representing as tags.                                                     *)
(* ------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------- *)
(* The default mapping.                                                      *)
(* ------------------------------------------------------------------------- *)

val default : show

end
