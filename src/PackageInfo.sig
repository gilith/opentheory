(* ========================================================================= *)
(* THEORY PACKAGE META-DATA                                                  *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageInfo =
sig

(* ------------------------------------------------------------------------- *)
(* A type of theory package meta-data.                                       *)
(* ------------------------------------------------------------------------- *)

type info

val mk : {name : PackageName.name, directory : string} -> info

val name : info -> PackageName.name

val directory : info -> {directory : string}

val joinDirectory : info -> {filename : string} -> {filename : string}

(* ------------------------------------------------------------------------- *)
(* Is the package properly installed?                                        *)
(* ------------------------------------------------------------------------- *)

val installed : info -> bool

(* ------------------------------------------------------------------------- *)
(* The files needed by the package.                                          *)
(* ------------------------------------------------------------------------- *)

val theoryFile : info -> {filename : string}

val files : info -> {filename : string} list

(* ------------------------------------------------------------------------- *)
(* Read the package.                                                         *)
(* ------------------------------------------------------------------------- *)

val package : info -> Package.package

end
