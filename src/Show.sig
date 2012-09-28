(* ========================================================================= *)
(* IMPORTING NAMESPACES FOR PRINTING PURPOSES                                *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
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

(* ------------------------------------------------------------------------- *)
(* The empty mapping.                                                        *)
(* ------------------------------------------------------------------------- *)

val natural : show

(* ------------------------------------------------------------------------- *)
(* Adding mappings.                                                          *)
(* ------------------------------------------------------------------------- *)

val add : show -> mapping -> show

val addList : show -> mapping list -> show

(* ------------------------------------------------------------------------- *)
(* Mapping names.                                                            *)
(* ------------------------------------------------------------------------- *)

val showNamespace : show -> Namespace.namespace -> Namespace.namespace

val showName : show -> Name.name -> Name.name

(* ------------------------------------------------------------------------- *)
(* Converting to/from mappings.                                              *)
(* ------------------------------------------------------------------------- *)

val toList : show -> mapping list

val fromList : mapping list -> show

(* ------------------------------------------------------------------------- *)
(* HTML output.                                                              *)
(* ------------------------------------------------------------------------- *)

val toHtmlMapping : mapping -> Html.inline list

val toHtml : show -> Html.inline list

(* ------------------------------------------------------------------------- *)
(* The default mapping.                                                      *)
(* ------------------------------------------------------------------------- *)

val default : show

end
