(* ========================================================================= *)
(* INSTALLED PACKAGE DIRECTORY                                               *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature DirectoryPackages =
sig

(* ------------------------------------------------------------------------- *)
(* A type of installed packages.                                             *)
(* ------------------------------------------------------------------------- *)

type packages

(* ------------------------------------------------------------------------- *)
(* Constructors and destructors.                                             *)
(* ------------------------------------------------------------------------- *)

val mk : {directory : string, filename : string} -> packages

val directory : packages -> {directory : string}

val filename : packages -> {filename : string}

val size : packages -> int

(* ------------------------------------------------------------------------- *)
(* Looking up packages.                                                      *)
(* ------------------------------------------------------------------------- *)

val peek : packages -> PackageName.name -> PackageInfo.info option

val member : PackageName.name -> packages -> bool

(* ------------------------------------------------------------------------- *)
(* Pretty-printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : packages Print.pp

val toString : packages -> string

end
