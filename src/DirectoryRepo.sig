(* ========================================================================= *)
(* PACKAGE DIRECTORY REPOSITORIES                                            *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature DirectoryRepo =
sig

(* ------------------------------------------------------------------------- *)
(* A type of repos.                                                          *)
(* ------------------------------------------------------------------------- *)

type name = string

type repo

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk : {name : name, url : string, filename : string} -> repo

val name : repo -> name

val url : repo -> {url : string}

val filename : repo -> {filename : string}

(* ------------------------------------------------------------------------- *)
(* Looking up packages.                                                      *)
(* ------------------------------------------------------------------------- *)

val peek : repo -> PackageName.name -> Checksum.checksum option

val member : PackageName.name -> repo -> bool

(* ------------------------------------------------------------------------- *)
(* Updating the package list.                                                *)
(* ------------------------------------------------------------------------- *)

val update : repo -> unit

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : repo Print.pp

val toString : repo -> string

end
