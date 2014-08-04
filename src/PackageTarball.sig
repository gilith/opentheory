(* ========================================================================= *)
(* PACKAGE TARBALLS                                                          *)
(* Copyright (c) 2010 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature PackageTarball =
sig

(* ------------------------------------------------------------------------- *)
(* Tarball filenames.                                                        *)
(* ------------------------------------------------------------------------- *)

val mkFilename : PackageNameVersion.nameVersion -> {filename : string}

val destFilename : {filename : string} -> PackageNameVersion.nameVersion option

val isFilename : {filename : string} -> bool

(* ------------------------------------------------------------------------- *)
(* Listing the contents.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype contents =
    Contents of
      {nameVersion : PackageNameVersion.nameVersion,
       theoryFile : {filename : string},
       otherFiles : {filename : string} list}

val contents : RepositorySystem.system -> {filename : string} -> contents

(* ------------------------------------------------------------------------- *)
(* Creating a checksum.                                                      *)
(* ------------------------------------------------------------------------- *)

val checksum :
    RepositorySystem.system -> {filename : string} -> Checksum.checksum

end
