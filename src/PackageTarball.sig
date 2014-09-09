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

val mk :
    {system : RepositorySystem.system,
     nameVersion : PackageNameVersion.nameVersion,
     checksum : Checksum.checksum option,
     filename : string} -> tarball

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

(* ------------------------------------------------------------------------- *)
(* Packing a tarball.                                                        *)
(* ------------------------------------------------------------------------- *)

val pack : tarball -> {filename : string} list -> unit

(* ------------------------------------------------------------------------- *)
(* Copying a tarball from a file.                                            *)
(* ------------------------------------------------------------------------- *)

val copy : tarball -> {filename : string} -> unit

(* ------------------------------------------------------------------------- *)
(* Downloading a tarball.                                                    *)
(* ------------------------------------------------------------------------- *)

val download : tarball -> {url : string} -> unit

(* ------------------------------------------------------------------------- *)
(* Extracting files from a tarball.                                          *)
(* ------------------------------------------------------------------------- *)

val extract : tarball -> {filename : string} list -> unit

(* ------------------------------------------------------------------------- *)
(* Uploading a tarball.                                                      *)
(* ------------------------------------------------------------------------- *)

val upload : tarball -> {url : string, token : string} -> {response : string}

end
