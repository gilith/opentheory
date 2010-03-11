(* ========================================================================= *)
(* IMPORTING NAMESPACES FOR PRINTING PURPOSES                                *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Show =
sig

(* ------------------------------------------------------------------------- *)
(* A type of namespace mapping.                                              *)
(* ------------------------------------------------------------------------- *)

datatype mapping =
    NamespaceMapping of Namespace.namespace * Namespace.namespace

(* ------------------------------------------------------------------------- *)
(* A type of namespace import map.                                           *)
(* ------------------------------------------------------------------------- *)

type show

val toList : show -> mapping list

(* ------------------------------------------------------------------------- *)
(* The empty map.                                                            *)
(* ------------------------------------------------------------------------- *)

val natural : show

(* ------------------------------------------------------------------------- *)
(* Adding mappings.                                                          *)
(* ------------------------------------------------------------------------- *)

val add : show -> mapping -> show

val addList : show -> mapping list -> show

val fromList : mapping list -> show

(* ------------------------------------------------------------------------- *)
(* Mapping namespaces.                                                       *)
(* ------------------------------------------------------------------------- *)

val showNamespace : show -> Namespace.namespace -> Namespace.namespace

val showName : show -> Name.name -> Name.name

end
