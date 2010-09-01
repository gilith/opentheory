(* ========================================================================= *)
(* PACKAGE TARBALLS                                                          *)
(* Copyright (c) 2010 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature PackageTarball =
sig

(* ------------------------------------------------------------------------- *)
(* Tarball filenames.                                                        *)
(* ------------------------------------------------------------------------- *)

val mkFilename : PackageName.name -> {filename : string}

val destFilename : {filename : string} -> PackageName.name option

val isFilename : {filename : string} -> bool

(* ------------------------------------------------------------------------- *)
(* Listing the contents.                                                     *)
(* ------------------------------------------------------------------------- *)

val contents :
    DirectoryConfig.system -> {filename : string} -> {filename : string} list

end
