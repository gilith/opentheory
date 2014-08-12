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
(* A type of package tarball.                                                *)
(* ------------------------------------------------------------------------- *)

type tarball

val mk : {system : RepositorySystem.system, filename : string} -> tarball

val filename : tarball -> {filename : string}

(* ------------------------------------------------------------------------- *)
(* Listing the contents.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype contents =
    Contents of
      {nameVersion : PackageNameVersion.nameVersion,
       theoryFile : {filename : string},
       otherFiles : {filename : string} list}

val contents : tarball -> contents

(* ------------------------------------------------------------------------- *)
(* Creating a checksum.                                                      *)
(* ------------------------------------------------------------------------- *)

val checksum : tarball -> Checksum.checksum

end
